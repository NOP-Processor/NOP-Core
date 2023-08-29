package NOP.pipeline.exe

import spinal.core._
import spinal.lib._

import NOP.blackbox.execute._
import NOP.utils._
import NOP.constants.enum._
import NOP.pipeline.core._
import NOP.builder._
import NOP.pipeline._
import NOP.pipeline.decode._
import NOP._

class MulDivExecutePlugin(val config: MyCPUConfig) extends Plugin[ExecutePipeline] {

  private val iqDepth = config.intIssue.depth
  private val rPorts = config.regFile.rPortsEachInst
  private val prfAddrWidth = config.regFile.prfAddrWidth

  val rrdReq = Vec(UInt(prfAddrWidth bits), rPorts)
  var rrdRsp: Vec[Bits] = null
  var clrBusy: Flow[UInt] = null
  var wPort: Flow[RegWriteBundle] = null
  var robWrite: Flow[UInt] = null

  object ISSUE_SLOT extends Stageable(MulDivIssueSlot(config))
  object REG_READ_RSP extends Stageable(Vec(BWord(), rPorts))
  object EXE_RESULT extends Stageable(Bits(32 bits))

  override def setup(pipeline: ExecutePipeline): Unit = {
    val PRF = pipeline.globalService(classOf[PhysRegFilePlugin])
    val ROB = pipeline.globalService(classOf[ROBFIFOPlugin])
    rrdRsp = Vec(rrdReq.map(PRF.readPort(_)))
    clrBusy = PRF.clearBusy
    wPort = PRF.writePort(true)
    robWrite = ROB.completePort
  }

  override def build(pipeline: ExecutePipeline): Unit = {
    val flush = pipeline.globalService(classOf[CommitPlugin]).regFlush
    pipeline plug new Area {
      // pipeline.stages.last.arbitration.flushIt setWhen flush
      pipeline.stages.drop(1).foreach(_.arbitration.removeIt setWhen flush)
    }

    // ! Issue
    pipeline.ISS plug new Area {
      import pipeline.ISS._
      val IQ = pipeline.globalService(classOf[MulDivIssueQueuePlugin])
      require(rPorts == 2)

      // ISSUE ONE INSTRUCTION
      val QUEUE_HEAD = 0
      val slot = IQ.queue(QUEUE_HEAD).payload
      val waken = (0 until rPorts).map { j => slot.rRegs(j).valid }.andR
      IQ.issueReq := IQ.queue(QUEUE_HEAD).valid && waken

      // one-hot插入issue slot，并设置valid
      val issSlot = insert(ISSUE_SLOT)
      issSlot := IQ.queue(QUEUE_HEAD).payload
      val issValid = IQ.issueReq
      IQ.issueFire clearWhen arbitration.isStuck
      arbitration.removeIt setWhen (arbitration.notStuck && (flush || !issValid))
    }

    // ! RRD
    pipeline.RRD plug new Area {
      import pipeline.RRD._
      val issSlot = input(ISSUE_SLOT)
      for (i <- 0 until rPorts) {
        rrdReq(i) := issSlot.rRegs(i).payload
        insert(REG_READ_RSP)(i) := rrdRsp(i)
      }

      arbitration.haltByOther setWhen pipeline
        .globalService(classOf[SpeculativeWakeupHandler])
        .regWakeupFailed
    }

    pipeline.EXE plug new Area {
      import pipeline.EXE._

      val issSlot = input(ISSUE_SLOT)
      val rrdRsp = input(REG_READ_RSP)
      val regData = rrdRsp
      val exeResult = insert(EXE_RESULT)

      val isSigned = issSlot.uop.signed

      // ! Multiply
      val isMultiply =
        arbitration.isValidOnEntry && (issSlot.uop.fuType === FUType.MUL || issSlot.uop.fuType === FUType.MULH)

      val rj = regData(0).asSInt
      val rk = regData(1).asSInt
      val absRj = rj.abs(isSigned)
      val absRk = rk.abs(isSigned)

      val wakeupCycle = CombInit(arbitration.notStuck)

      val multiplier = new Multiplier()
      multiplier.io.A := absRj
      multiplier.io.B := absRk
      val mulResult =
        multiplier.io.P.twoComplement(isSigned && (rj.sign ^ rk.sign)).asBits(0, 64 bits)

      val mulCounter = Counter(config.mulDiv.multiplyLatency + 1)

      when(isMultiply) {
        mulCounter.increment()
        arbitration.haltItself setWhen (!mulCounter.willOverflowIfInc)
        // assume never halt by others
        wakeupCycle := mulCounter === config.mulDiv.multiplyLatency - 1
      }

      // ! Division
      val isDivision = arbitration.isValidOnEntry &&
        (issSlot.uop.fuType === FUType.DIV || issSlot.uop.fuType === FUType.MOD)
      val isFirstCycle = RegNext(arbitration.notStuck)
      val useEarlyOut = config.mulDiv.useDivisionEarlyOut
      val smallDivSize = config.mulDiv.divisionEarlyOutWidth
      val in16Bits =
        if (useEarlyOut)
          absRj(31 downto smallDivSize) === 0 && absRk(31 downto smallDivSize) === 0
        else
          False
      val (quotient, remainder) = {
        val divider = new math.UnsignedDivider(32, 32, false)
        divider.io.flush := False
        divider.io.cmd.valid := isDivision && isFirstCycle && !in16Bits
        divider.io.cmd.numerator := absRj
        divider.io.cmd.denominator := absRk
        divider.io.rsp.ready := !arbitration.isStuckByOthers
        val absQuotient = UInt(32 bits)
        val absRemainder = UInt(32 bits)

        if (useEarlyOut) {
          val divider16 = new math.UnsignedDivider(16, 16, false)
          divider16.io.flush := False
          divider16.io.cmd.valid := isDivision && isFirstCycle
          divider16.io.cmd.numerator := absRj.resized
          divider16.io.cmd.denominator := absRk.resized
          divider16.io.rsp.ready := !arbitration.isStuckByOthers

          when(isDivision) {
            arbitration.haltItself setWhen (!in16Bits && !divider.io.rsp.valid)
            arbitration.haltItself setWhen (in16Bits && !divider16.io.rsp.valid)
          }
          when(in16Bits) {
            absQuotient := divider16.io.rsp.quotient.resized
            absRemainder := divider16.io.rsp.remainder.resized
          } otherwise {
            absQuotient := divider.io.rsp.quotient
            absRemainder := divider.io.rsp.remainder
          }
        } else {
          when(isDivision) {
            arbitration.haltItself setWhen (!divider.io.rsp.valid)
          }
          absQuotient := divider.io.rsp.quotient
          absRemainder := divider.io.rsp.remainder
        }
        val quotient = absQuotient.twoComplement(isSigned && (rj.sign ^ rk.sign)).asBits(0, 32 bits)
        val remainder = absRemainder.twoComplement(isSigned && rj.sign).asBits(0, 32 bits)
        (quotient, remainder)
      }

      exeResult.assignDontCare()
      switch(issSlot.uop.fuType) {
        is(FUType.MUL) {
          exeResult := mulResult(31 downto 0)
        }
        is(FUType.MULH) {
          exeResult := mulResult(63 downto 32)
        }
        is(FUType.DIV) {
          exeResult := quotient
        }
        is(FUType.MOD) {
          exeResult := remainder
        }
      }

      // 远程唤醒
      clrBusy.valid := arbitration.isValid && wakeupCycle && issSlot.uop.doRegWrite
      clrBusy.payload := issSlot.wReg
    }

    pipeline.WB plug new Area {
      import pipeline.WB._
      val issSlot = input(ISSUE_SLOT)
      val exeResult = input(EXE_RESULT)
      val robIdx = issSlot.robIdx

      wPort.valid := arbitration.isValidNotStuck && issSlot.uop.doRegWrite
      wPort.payload.addr := issSlot.wReg
      wPort.payload.data := exeResult

      robWrite.valid := arbitration.isValidNotStuck
      robWrite.payload := robIdx

    }

  }

}
