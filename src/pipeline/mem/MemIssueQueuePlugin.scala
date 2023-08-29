package NOP.pipeline.exe

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.utils._
import NOP.pipeline._
import NOP.pipeline.core._
import NOP.constants.enum._

class MemIssueQueuePlugin(config: MyCPUConfig)
    extends CompressedFIFO(
      config.memIssue,
      config.decode.decodeWidth,
      HardType(MemIssueSlot(config))
    ) {
  private val issConfig = config.memIssue
  val rPorts = config.regFile.rPortsEachInst
  val busyAddrs = Vec(UInt(config.regFile.prfAddrWidth bits), decodeWidth * rPorts)
  def fuMatch(uop: MicroOp) = uop.fuType === FUType.LSU

  def build(pipeline: MyCPUCore): Unit = pipeline.DISPATCH plug new Area {
    genIssueSelect()
    genGlobalWakeup(pipeline.service(classOf[PhysRegFilePlugin]), rPorts)
    import pipeline.DISPATCH._
    // 入队、唤醒与int IQ相同
    val decPacket = input(pipeline.decodePipeline.signals.DECODE_PACKET)
    val renameRecs = input(pipeline.decodePipeline.signals.RENAME_RECORDS)
    val robIdxs = input(pipeline.decodePipeline.signals.ROB_INDEXES)
    val enqueueMask = Bits(decodeWidth bits)
    queueIO.pushPorts.foreach(_.setIdle())
    for (i <- 0 until decodeWidth) {
      val valid = decPacket(i).valid
      val uop = decPacket(i).payload
      val rename = renameRecs(i)
      val pushPort = queueIO.pushPorts.dataType().setBlocked()
      val slot = pushPort.payload

      slot.uop.assignSomeByName(uop)
      slot.uop.immField := uop.inst(21 downto 10).asSInt.resize(32 bits).asBits
      when(slot.uop.isSC || slot.uop.isLL) {
        slot.uop.immField := (uop.inst(23 downto 10) ## U(0x0, 2 bits)).asSInt.resize(32 bits).asBits
      }

      // ! Special support decoding for CACOP and PRELD
      switch(uop.lsType) {
        is(LoadStoreType.CACOP) {
          switch(uop.inst(4 downto 3)) {
            is(0x0) {
              slot.uop.cacheOp := CacheOpType.StoreTag
            }
            is(0x1) {
              slot.uop.cacheOp := CacheOpType.IndexInvalidate
            }
            is(0x2) {
              slot.uop.cacheOp := CacheOpType.HitInvalidate
            }
            default {
              slot.uop.cacheOp := CacheOpType.None
            }
          }

          switch(uop.inst(2 downto 0)) {
            is(0x0) {
              slot.uop.cacheSel := CacheSelType.ICache
            }
            is(0x1) {
              slot.uop.cacheSel := CacheSelType.DCache
            }
            default {
              slot.uop.cacheSel := CacheSelType.None
            }
          }
        }
        is(LoadStoreType.PRELD) {
          slot.uop.cacheOp := CacheOpType.None
          switch(uop.inst(4 downto 0)) {
            is(0x0) {
              slot.uop.cacheSel := CacheSelType.ICache
            }
            is(0x8) {
              slot.uop.cacheSel := CacheSelType.DCache
            }
            default {
              slot.uop.cacheSel := CacheSelType.None
            }
          }

        }
        default {
          slot.uop.cacheOp := CacheOpType.None
          slot.uop.cacheSel := CacheSelType.DCache
        }
      }

      for (j <- 0 until rPorts) {
        slot.rRegs(j).payload := rename.rRegs(j)
        busyAddrs(i * rPorts + j) := rename.rRegs(j)
      }
      // 入队唤醒（dispatch入口读busy）
      slot.rRegs(0).valid := !uop.useRj || !busyRsps(i * rPorts)
      slot.rRegs(1).valid := !(uop.useRk || uop.useRd) || !busyRsps(i * rPorts + 1)
      slot.wReg := rename.wReg
      slot.robIdx := robIdxs(i)
      val enqueue = valid && fuMatch(uop) && !uop.except.valid
      enqueueMask(i) := enqueue
      pushPort.valid := arbitration.isValidNotStuck && enqueue
      arbitration.haltItself setWhen (arbitration.isValid && enqueue && !pushPort.ready)

      // port互联，与rename相似
      if (i > 0) {
        val pushIdx = CountOne(enqueueMask.take(i))
        for (j <- 0 to i) when(pushIdx === j && enqueueMask(i))(pushPort >> queueIO.pushPorts(j))
      } else {
        when(enqueueMask(i))(pushPort >> queueIO.pushPorts(0))
      }
    }

    // flush最高优先级
    val flush = pipeline.globalService(classOf[CommitPlugin]).regFlush
    queueFlush setWhen flush

  }
  Component.current.afterElaboration {
    genEnqueueLogic()
    genCompressLogic()
    genFlushLogic()
  }
}
