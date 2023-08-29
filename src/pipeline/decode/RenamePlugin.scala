package NOP.pipeline.decode

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.utils._
import NOP.pipeline._
import NOP.pipeline.core._

class RenamePlugin(config: MyCPUConfig) extends Plugin[DecodePipeline] {
  private val rfConfig = config.regFile
  private val decodeWidth = config.decode.decodeWidth
  private val retireWidth = config.rob.retireWidth
  private val arfAddrWidth = rfConfig.arfAddrWidth
  private val prfAddrWidth = rfConfig.prfAddrWidth
  private val prfAddrType = HardType(UInt(prfAddrWidth bits))
  // speculative register alias table
  val sRAT = Vec.tabulate(rfConfig.nArchRegs - 1) { i => RegInit(U(i + 1, prfAddrWidth bits)) }

  // architecture register alias table
  val aRAT = Vec.tabulate(rfConfig.nArchRegs - 1) { i => RegInit(U(i + 1, prfAddrWidth bits)) }
  // freeList永远不可能超过满，因为ARF必然占用其中的位置
  val freeList = new MultiPortFIFOVecOutReg(
    prfAddrType,
    rfConfig.nPhysRegs + 1 - rfConfig.nArchRegs,
    retireWidth,
    decodeWidth,
    dataInit = { i => U(i + rfConfig.nArchRegs, prfAddrWidth bits) },
    initFull = true
  ) {
    val recover = in(Bool)
    when(recover) {
      // 非满情况下popPtr->pushPtr是有效数据，将其恢复为满，相当于所有最新pop的都回滚了。
      // 而这些pop的正是rename占用的speculative registers，数量也正好等于没有retire的写数量。
      // 注意到每一个写都会导致rename的时候pop一个新reg作为speculative，retire的时候push旧的，
      // 旧的从architecture变成free，新的从speculative变成architecture。
      // Remark. 应当观察到如下事情：每一条写指令pop free list和push free list的ptr值是一样的。
      // 重要！以下的修改fix掉了recover之后一个周期的pop payload。但要求recover周期不能动push和pop。
      // popPtr := 0
      // pushPtr := 0
      pushPtr := popPtr
      isRisingOccupancy := True
    }
  }

  override def build(pipeline: DecodePipeline): Unit = pipeline.RENAME plug new Area {
    import pipeline.RENAME._

    // TODO: [NOP] remove this debugging code
    val debug_sRAT = out(Bits(prfAddrWidth * (rfConfig.nArchRegs - 1) bits))
    val debug_aRAT = out(Bits(prfAddrWidth * (rfConfig.nArchRegs - 1) bits))
    debug_sRAT := sRAT.asBits
    debug_aRAT := aRAT.asBits
    pipeline.update_signal("sRAT", debug_sRAT)
    pipeline.update_signal("aRAT", debug_aRAT)

    val decPacket = input(pipeline.signals.DECODE_PACKET)
    assert(rfConfig.rPortsEachInst == 2)
    def portMatch(dest: RegRenameBundle, src: RegRenameBundle) =
      src.req.valid && src.req.payload === dest.req.payload
    val regReads =
      Vec(
        Vec(RegRenameBundle(arfAddrWidth, prfAddrWidth), rfConfig.rPortsEachInst + 1),
        decodeWidth
      )
    val regWrites =
      Vec(RegRenameBundle(arfAddrWidth, prfAddrWidth), decodeWidth)
    val noFreeRegs = False
    freeList.io.pop.foreach(_.setBlocked())
    for (i <- 0 until decPacket.size; valid = decPacket(i).valid; uop = decPacket(i).payload) {
      val fields = InstructionParser(uop.inst)
      regReads(i)(0).req.valid := uop.useRj && valid
      regReads(i)(0).req.payload := fields.rj
      regReads(i)(1).req.valid := (uop.useRk || uop.useRd) && valid
      regReads(i)(1).req.payload := Mux(uop.useRk, fields.rk, fields.rd)
      val wPort = regWrites(i)
      wPort.req.valid := uop.doRegWrite && valid
      wPort.req.payload := uop.wbAddr
      // 将写地址添加到读，读出来的就是此逻辑位置上旧的映射
      regReads(i)(2).req := wPort.req
      regReads(i).foreach { rPort =>
        // 无相关性，默认读RAT
        when(rPort.req.payload === 0) {
          rPort.rsp := 0
        } otherwise {
          rPort.rsp := sRAT(rPort.req.payload - 1)
        }
        // 前面的写match本次读，RAW相关性，读最新值
        i > 0 generate regWrites.take(i).foreach { p =>
          when(portMatch(rPort, p)) {
            rPort.rsp := p.rsp
          }
        }
      }
      // pop出一个空闲寄存器
      val popPort = freeList.io.pop.dataType().setCompositeName(wPort, "popPort")
      popPort.setIdle()
      if (i > 0) {
        // 互联pop口，保证pop连续性
        val popIdx =
          CountOne(regWrites.take(i).map(_.req.valid)).setCompositeName(wPort, "popIdx")
        for (j <- 0 to i)
          when(popIdx === j && regWrites(i).req.valid)(popPort <> freeList.io.pop(j))
      } else {
        when(regWrites(i).req.valid)(popPort <> freeList.io.pop(0))
      }
      popPort.ready := arbitration.isValidNotStuck && wPort.req.valid
      // freeList空了
      noFreeRegs setWhen (arbitration.isValid && wPort.req.valid && !popPort.valid)
      wPort.rsp := popPort.payload
      // 后覆盖前，解决了WAW相关性
      when(arbitration.isValidNotStuck && wPort.req.valid)(sRAT(wPort.req.payload - 1) := wPort.rsp)
      // RENAME结果插入流水线（给ROB用）
      import pipeline.signals.RENAME_RECORDS
      for (j <- 0 until rfConfig.rPortsEachInst) {
        insert(RENAME_RECORDS)(i).rRegs(j) := regReads(i)(j).rsp
      }
      insert(RENAME_RECORDS)(i).wPrevReg := regReads(i)(2).rsp
      insert(RENAME_RECORDS)(i).wReg := regWrites(i).rsp
    }
    arbitration.haltItself setWhen noFreeRegs

    val arfCommit = pipeline.globalService(classOf[CommitPlugin])
    // 提交时，修改aRAT并释放sRAT
    freeList.io.push.foreach(_.setIdle())
    for (i <- 0 until retireWidth; commit = arfCommit.arfCommits(i)) {
      val port = freeList.io.push.dataType().setCompositeName(commit, "pushPort").setBlocked()
      if (i > 0) {
        // 互联push口，保证push连续性
        val pushIdx =
          CountOne(arfCommit.arfCommits.take(i).map(_.valid)).setCompositeName(commit, "pushIdx")
        for (j <- 0 to i)
          when(pushIdx === j && commit.valid)(port <> freeList.io.push(j))
      } else {
        when(commit.valid)(port <> freeList.io.push(0))
      }
      port.valid := commit.valid
      port.payload := commit.payload.prevAddr
      when(commit.valid) {
        aRAT(commit.payload.addr - 1) := commit.payload.prfAddr
      }
    }
    freeList.io.push.drop(retireWidth).foreach(_.setIdle())
    // 分支预测恢复时，将aRAT拷贝进sRAT
    when(arfCommit.recoverPRF) {
      sRAT := aRAT
    }
    // 预测恢复时freeList也要恢复
    freeList.recover := arfCommit.recoverPRF
  }
}
