package NOP.pipeline.exe

import spinal.core._
import spinal.lib._
import NOP.utils._
import NOP.constants.enum._
import NOP.pipeline.core._
import NOP.builder._
import NOP.pipeline._
import NOP.pipeline.decode._
import NOP._
import NOP.constants.LoongArch.CSRAddress
import NOP.pipeline.priviledge.CSRPlugin

class IntExecutePlugin(val config: MyCPUConfig, val fuIdx: Int) extends Plugin[ExecutePipeline] {

  private val withBRU = config.intIssue.bruIdx == fuIdx
  private val withCSR = config.intIssue.csrIdx == fuIdx
  private val withTimer = config.intIssue.timerIdx == fuIdx
  private val withINVTLB = config.intIssue.invTLBIdx == fuIdx

  private val iqDepth = config.intIssue.depth
  private val rPorts = config.regFile.rPortsEachInst
  private val prfAddrWidth = config.regFile.prfAddrWidth

  val rrdReq = Vec(UInt(prfAddrWidth bits), rPorts)
  var rrdRsp: Vec[Bits] = null
  val bypassReq = Vec(UInt(prfAddrWidth bits), rPorts)
  var bypassRsp: Vec[Flow[Bits]] = null
  var clrBusy: Flow[UInt] = null
  var wPort: Flow[RegWriteBundle] = null
  var robWriteBRU: Flow[ROBStateBRUPortBundle] = null
  var robWrite: Flow[ROBStateALUPortBundle] = null

  object ISSUE_SLOT extends Stageable(IntIssueSlot(config))
  object REG_READ_RSP extends Stageable(Vec(BWord(), rPorts))
  object EXE_RESULT extends Stageable(BWord())
  object WRITE_REG extends Stageable(Flow(UInt(prfAddrWidth bits)))
  object ROB_IDX extends Stageable(UInt(config.rob.robAddressWidth bits))

  object DIFF_IS_COUNT extends Stageable(Bool())
  object DIFF_COUNT64_value extends Stageable(UInt(64 bits))
  object DIFF_CSR_RSTAT extends Stageable(Bool())
  object DIFF_CSR_DATA extends Stageable(UWord())

  object ACTUAL_TARGET extends Stageable(UWord())
  object ACTUAL_TAKEN extends Stageable(Bool)
  object MISPREDICT extends Stageable(Bool)

  val issReqs = Vec(Bool, iqDepth)
  var issGrant: Vec[Bool] = null

  override def setup(pipeline: ExecutePipeline): Unit = {
    val PRF = pipeline.globalService(classOf[PhysRegFilePlugin])
    val IQ = pipeline.globalService(classOf[IntIssueQueuePlugin])
    val ROB = pipeline.globalService(classOf[ROBFIFOPlugin])

    rrdRsp = Vec(rrdReq.map(PRF.readPort(_)))
    clrBusy = PRF.clearBusy
    wPort = PRF.writePort(true)

    // Logic: (1) IntPipeline detects all issue slots in issue queue that can be issued
    //        (2) IntPipeline sends issue requests to IntIssueQueue
    //        (3) IntIssueQueue sends issue grants to IntPipeline
    issGrant = IQ.grantPort(issReqs)

    // RRD: Read PRF
    // EXE: Read from Bypass Network. If not found, use RRD response
    val BYPASS = pipeline.globalService(classOf[BypassNetworkPlugin])
    BYPASS.writePorts += wPort
    bypassRsp = Vec(bypassReq.map(BYPASS.readPort(_)))

    // ! Hook with interface of ROB
    if (withBRU || withCSR || withINVTLB) robWriteBRU = ROB.bruPort
    else robWrite = ROB.aluPort
  }

  override def build(pipeline: ExecutePipeline): Unit = {
    val flush = pipeline.globalService(classOf[CommitPlugin]).regFlush

    pipeline plug new Area {
      // pipeline.stages.last.arbitration.flushIt setWhen flush
      pipeline.stages.drop(1).foreach(_.arbitration.removeIt setWhen flush)
    }

    pipeline.ISS plug new Area {
      import pipeline.ISS._
      val IQ = pipeline.globalService(classOf[IntIssueQueuePlugin])
      require(rPorts == 2)

      val validVec = Vec(Bool, iqDepth)
      val wakenVec = Vec(Bool, iqDepth)
      val matchVec = Vec(Bool, iqDepth)

      for (i <- 0 until iqDepth) {
        val slot = IQ.queue(i).payload // issue slot
        val waken = (0 until rPorts).map { j => slot.rRegs(j).valid }.andR
        val fuMatch = True

        if (!withBRU) fuMatch clearWhen slot.uop.fuType === FUType.CMP
        if (!withCSR) fuMatch clearWhen slot.uop.fuType === FUType.CSR
        if (!withTimer) fuMatch clearWhen slot.uop.fuType === FUType.TIMER
        if (!withINVTLB) fuMatch clearWhen slot.uop.fuType === FUType.INVTLB

        // 计算每个槽是否valid
        issReqs(i) := IQ.queue(i).valid && fuMatch && waken

        validVec(i) := IQ.queue(i).valid
        wakenVec(i) := waken
        matchVec(i) := fuMatch
      }

      // one-hot插入issue slot，并设置valid
      val issSlot = insert(ISSUE_SLOT)
      issSlot := MuxOH(issGrant, IQ.queue.map(_.payload))

      val issValid = issGrant.orR
      IQ.issueFire clearWhen arbitration.isStuck
      arbitration.removeIt setWhen (arbitration.notStuck && (flush || !issValid))

      // 本地 bypass 唤醒，早一个周期，一个周期后既能出结果，该指令也能 RRD
      when(issValid && issSlot.uop.doRegWrite) {
        for (i <- 0 until iqDepth) {
          for (j <- 0 until rPorts)
            when(issSlot.wReg === IQ.queueNext(i).rRegs(j).payload && arbitration.notStuck) {
              // bypass wake-up with bypass network
              IQ.queue(i).rRegs(j).valid := True
              IQ.queueNext(i).rRegs(j).valid := True
            }
        }
      }
    }

    pipeline.RRD plug new Area {
      import pipeline.RRD._
      val issSlot = input(ISSUE_SLOT)
      for (i <- 0 until rPorts) {
        rrdReq(i) := issSlot.rRegs(i).payload
        insert(REG_READ_RSP)(i) := rrdRsp(i)
      }
      // 远程唤醒（genGlobalWakeup）
      // 在这个周期，当前指令指示下一周期清掉了 wReg 的 Busy
      // 下一周期，在 IssueQueue 中使用该寄存器的指令的寄存器变成 valid 状态，可以被 ISS
      // 再下一周期，本指令进入 WB 阶段，而使用该寄存器的指令进入 RRD 阶段，因此需要旁路
      clrBusy.valid := arbitration.isValidNotStuck && issSlot.uop.doRegWrite
      clrBusy.payload := issSlot.wReg

      arbitration.haltByOther setWhen pipeline
        .globalService(classOf[SpeculativeWakeupHandler])
        .regWakeupFailed

      // TODO: [NOP] DiffTest Bundle. Remove this in the future.
      insert(DIFF_IS_COUNT) := False
      insert(DIFF_COUNT64_value) := 0
      insert(DIFF_CSR_RSTAT) := False
      insert(DIFF_CSR_DATA) := 0
    }

    pipeline.EXE plug new Area {
      import pipeline.EXE._
      val issSlot = input(ISSUE_SLOT)
      val alu = new ALU()

      // ! Prepare input data
      val rrdRsp = input(REG_READ_RSP)
      val regData = CombInit(rrdRsp)
      // bypass logic
      for (i <- 0 until rPorts) {
        bypassReq(i) := issSlot.rRegs(i).payload
        when(bypassRsp(i).valid) { regData(i) := bypassRsp(i).payload }
      }
      // Now regData is the data read from bypass network or PRF

      // ! Calculate return value
      val exeResult = insert(EXE_RESULT) // return value

      // TODO: [NOP] remove this debug signal
      val debug_exeResult = out(BWord())
      debug_exeResult := exeResult
      pipeline.update_signal("exeResult", debug_exeResult)

      val fields = InstructionParser(issSlot.uop.inst)
      val imm12 = issSlot.uop.immExtendType
        .mux(
          ExtendType.SIGN -> fields.signExtendImm12,
          ExtendType.ZERO -> fields.zeroExtendImm12
        )

      val alu_src1_alternative = UInt(32 bits)
      switch(issSlot.uop.aluOp) {
        is(ALUOpType.LU12I) {
          alu_src1_alternative := U(0x0, 32 bits)
        }
        is(ALUOpType.PCADDI) {
          alu_src1_alternative := issSlot.uop.pc
        }
        is(ALUOpType.PCADDU12I) {
          alu_src1_alternative := issSlot.uop.pc
        }
        default {
          alu_src1_alternative := U(0xdeadbeefL, 32 bits)
        }
      }
      alu.io.src1 := Mux(issSlot.uop.useRj, regData(0).asUInt, alu_src1_alternative)

      val alu_src2_alternative = UInt(32 bits)
      switch(issSlot.uop.aluOp) {
        is(ALUOpType.LU12I) {
          alu_src2_alternative := fields.signExtendImm20_12
        }
        is(ALUOpType.PCADDI) {
          alu_src2_alternative := fields.signExtendImm20_2
        }
        is(ALUOpType.PCADDU12I) {
          alu_src2_alternative := fields.signExtendImm20_12
        }
        default {
          alu_src2_alternative := imm12
        }
      }
      alu.io.src2 := Mux(issSlot.uop.useRk, regData(1).asUInt, alu_src2_alternative)

      alu.io.sa := Mux(issSlot.uop.useRk, regData(1).asUInt(4 downto 0), fields.sa)
      alu.io.op := issSlot.uop.aluOp

      // set default result
      exeResult := alu.io.result.asBits

      // ! BRU
      if (withBRU) {
        // BRU Section
        val bru = new BRU
        val comparator = new Comparator()
        comparator.io.src1 := regData(0).asUInt
        comparator.io.src2 := Mux(issSlot.uop.useRd, regData(1).asUInt, imm12)
        comparator.io.op := issSlot.uop.cmpOp

        bru.io.predictJump := issSlot.uop.predInfo.predictTaken
        bru.io.predictAddr := issSlot.uop.predInfo.predictAddr
        bru.io.isBranch := issSlot.uop.isBranch
        bru.io.isJR := issSlot.uop.isJR // JIRL
        bru.io.isJump := issSlot.uop.isJump // B or BL
        bru.io.pc := issSlot.uop.pc
        bru.io.inst := issSlot.uop.inst
        bru.io.condition := comparator.io.result
        bru.io.rj := regData(0).asUInt

        insert(ACTUAL_TARGET) := bru.io.actualTarget
        insert(ACTUAL_TAKEN) := comparator.io.result || issSlot.uop.isJR || issSlot.uop.isJump
        insert(MISPREDICT) := bru.io.mispredict

        // Just set exeResult is okay!
        when(issSlot.uop.isJump || issSlot.uop.isJR) {
          exeResult := (issSlot.uop.pc + UWord(4)).asBits
        }
      }

      if (withCSR) {
        val CSRPlugin = pipeline.globalService(classOf[CSRPlugin])
        CSRPlugin.readAddress := fields.csrAddr

        when(issSlot.uop.readCSR) {
          exeResult := CSRPlugin.readData
          output(DIFF_CSR_RSTAT) := fields.csrAddr === CSRAddress.ESTAT
          output(DIFF_CSR_DATA) := CSRPlugin.readData.asUInt
        }

        when(issSlot.uop.writeCSR) {
          when(issSlot.uop.useRj) {
            insert(ACTUAL_TARGET) := (regData(0).asUInt & regData(1).asUInt) | (~regData(
              0
            ).asUInt & CSRPlugin.readData.asUInt)
          } otherwise {
            insert(ACTUAL_TARGET) := regData(1).asUInt
          }
        }
      }

      // Timer
      if (withTimer) {
        val timer64Plugin = pipeline.globalService(classOf[Timer64Plugin])
        when(issSlot.uop.readTimer64ID) {
          exeResult := timer64Plugin.counterId.asBits
          output(DIFF_IS_COUNT) := True
        }
        when(issSlot.uop.readTimer64L) {
          exeResult := timer64Plugin.lowBits.asBits
          output(DIFF_IS_COUNT) := True
        }
        when(issSlot.uop.readTimer64H) {
          exeResult := timer64Plugin.highBits.asBits
          output(DIFF_IS_COUNT) := True
        }
        insert(DIFF_COUNT64_value) := (timer64Plugin.highBits @@ timer64Plugin.lowBits)
      }

      // INVTLB
      if (withINVTLB) {
        when(issSlot.uop.operateTLB) {
          val result = insert(ACTUAL_TARGET)
          result(9 downto 0) := regData(0).asUInt(9 downto 0) // ASID
          result(28 downto 10) := regData(1).asUInt(31 downto 13) // VPN
          result(31 downto 29) := 0
        }
      }

      insert(WRITE_REG).valid := issSlot.uop.doRegWrite
      insert(WRITE_REG).payload := issSlot.wReg
      insert(ROB_IDX) := issSlot.robIdx
    }

    pipeline.WB plug new Area {
      import pipeline.WB._
      val wbReq = input(WRITE_REG)
      val wbData = input(EXE_RESULT)
      val robIdx = input(ROB_IDX)

      // ! Write Back: directly write back to PRF...
      wPort.valid := arbitration.isValidNotStuck && wbReq.valid
      wPort.payload.addr := wbReq.payload
      wPort.payload.data := wbData

      // No except can occur in IntExecute
      val except = Flow(ExceptionPayloadBundle(false)).setIdle()

      // ! Commit
      if (withBRU || withCSR) {
        robWriteBRU.valid := arbitration.isValidNotStuck
        robWriteBRU.robIdx := robIdx
        robWriteBRU.except := except
        robWriteBRU.intResult := input(ACTUAL_TARGET)
        robWriteBRU.mispredict := input(MISPREDICT)
        robWriteBRU.actualTaken := input(ACTUAL_TAKEN)
      } else {
        robWrite.valid := arbitration.isValidNotStuck
        robWrite.robIdx := robIdx
        robWrite.except := except
        robWrite.intResult := wbData.asUInt
        robWrite.isCount := input(DIFF_IS_COUNT) && robWrite.valid
        robWrite.count64ReadValue := input(DIFF_COUNT64_value)
        robWrite.csrRstat := input(DIFF_CSR_RSTAT) && robWrite.valid
        robWrite.csrRdata := input(DIFF_CSR_DATA)
        robWrite.myPC := input(ISSUE_SLOT).uop.pc
      }
    }
  }
}
