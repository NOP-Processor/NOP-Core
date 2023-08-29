package NOP.pipeline.exe

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.utils._
import NOP.pipeline._
import NOP.pipeline.core._
import NOP.constants.enum._

/** 整数issue queue，不包括乘除法。采用压缩方法。
  */
class IntIssueQueuePlugin(config: MyCPUConfig)
    extends CompressedQueue(
      config.intIssue,
      config.decode.decodeWidth,
      HardType(IntIssueSlot(config))
    ) {
  private val issConfig = config.intIssue
  val rPorts = config.regFile.rPortsEachInst
  val busyAddrs = Vec(UInt(config.regFile.prfAddrWidth bits), decodeWidth * rPorts)
  def fuMatch(uop: MicroOp): Bool = {
    uop.fuType === FUType.ALU || uop.fuType === FUType.CMP ||
    uop.fuType === FUType.CSR || uop.fuType === FUType.TIMER || uop.fuType === FUType.INVTLB
  }

  // decode index
  object PUSH_INDEXES extends Stageable(Vec(Flow(UInt(log2Up(decodeWidth) bits)), decodeWidth))

  override def build(pipeline: MyCPUCore): Unit = {
    // RENAME
    pipeline.RENAME plug new Area {
      import pipeline.RENAME._
      val decPacket = input(pipeline.decodePipeline.signals.DECODE_PACKET)
      val enqueueMask = Bits(decodeWidth bits)
      val pushIndexes = insert(PUSH_INDEXES)
      for (i <- 0 until decodeWidth) {
        val valid = decPacket(i).valid
        val uop = decPacket(i).payload
        enqueueMask(i) := valid && fuMatch(uop) && !uop.except.valid // say, 110100
        // 在rename阶段计算互联
        pushIndexes(i).setIdle()
        if (i > 0) {
          val pushIdx = CountOne(enqueueMask.take(i)) // 1, 2, 2, 3, 3, 3 for i = 0, 1, 2, 3, 4, 5
          for (j <- 0 to i) when(pushIdx === j && enqueueMask(i))(pushIndexes(j).push(i)) // pushIndexes[pushIdx] = i
        } else {
          when(enqueueMask(i))(pushIndexes(0).push(i))
        }
      }
    }

    // DISPATCH
    pipeline.DISPATCH plug new Area {
      genIssueSelect()
      genGlobalWakeup(pipeline.service(classOf[PhysRegFilePlugin]), rPorts)
      import pipeline.DISPATCH._
      // 唤醒逻辑：
      // 1. 入队唤醒（dispatch入口读busy）
      // 2. 远程唤醒（监听写busy广播）
      // 3. 本地唤醒（监听select结果）
      // 入队
      val decPacket = input(pipeline.decodePipeline.signals.DECODE_PACKET)
      val renameRecs = input(pipeline.decodePipeline.signals.RENAME_RECORDS)
      val robIdxs = input(pipeline.decodePipeline.signals.ROB_INDEXES)
      val pushIndexes = input(PUSH_INDEXES)
      val pushPorts = Vec(slotType, decodeWidth)
      for (i <- 0 until decodeWidth) {
        // slot处理
        val valid = decPacket(i).valid
        val uop = decPacket(i).payload
        val rename = renameRecs(i)
        val slot = pushPorts(i)
        slot.uop.assignSomeByName(uop)

        for (j <- 0 until rPorts) {
          slot.rRegs(j).payload := rename.rRegs(j)
          busyAddrs(i * rPorts + j) := rename.rRegs(j)
        }
        // 入队唤醒（dispatch入口读busy）
        slot.rRegs(0).valid := !uop.useRj || !busyRsps(i * rPorts)
        slot.rRegs(1).valid := !(uop.useRk || uop.useRd) || !busyRsps(i * rPorts + 1)
        slot.wReg := rename.wReg
        slot.robIdx := robIdxs(i)

        // port互联，与rename相似
        val idx = pushIndexes(i)
        val port = queueIO.pushPorts(i)
        port.valid := arbitration.isValidNotStuck && idx.valid
        port.payload := pushPorts(idx.payload)
        arbitration.haltItself setWhen (arbitration.isValid && idx.valid && !port.ready)
      }

      // flush最高优先级
      val flush = pipeline.globalService(classOf[CommitFlush]).regFlush
      queueFlush setWhen flush
    }

    Component.current.afterElaboration {
      genEnqueueLogic()
      genCompressLogic()
      genFlushLogic()
    }
  }
}
