package NOP.pipeline.mem

import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.pipeline._
import NOP.constants.enum._
import NOP.pipeline.core.CommitPlugin

final case class StoreBufferSlot(config: MyCPUConfig) extends Bundle {
  val retired = Bool()
  val addr = UWord()
  val be = Bits(4 bits)
  val data = BWord()
  // extension: accept uncached load/store
  val isStore = Bool()
  val isCached = Bool()
  val wReg = Flow(UInt(config.regFile.prfAddrWidth bits))
  val lsType = LoadStoreType()
  val robIdx = UInt(config.rob.robAddressWidth bits)
}

/** Store buffer. 压缩式FIFO。
  *
  * @param config
  */
class StoreBufferPlugin(config: MyCPUConfig) extends Plugin[MemPipeline] {
  val depth = config.storeBufferDepth
  val slotType = HardType(StoreBufferSlot(config))
  val queue = Vec(RegFlow(slotType()), depth)
  val queueNext = CombInit(queue)
  val queueIO = new Area {
    val pushPort = Stream(slotType)
    val popPort = Stream(slotType).setBlocked()
    popPort.valid := queue(0).valid
    popPort.payload := queue(0).payload
  }

  // Bypassing query logic
  val query = new Area {
    val addr = UWord() // Input
    val data = BWord().assignDontCare() // Output
    val be = B(0, 4 bits) // Output
    // 新覆盖老
    for (i <- 0 until depth)
      when(
        addr(2, 30 bits) === queue(i).addr(2, 30 bits) &&
          queue(i).valid && queue(i).isStore && queue(i).isCached
      ) {
        for (j <- 0 until 4)
          when(queue(i).be(j)) {
            be(j) := True
            data(j * 8, 8 bits) := queue(i).data(j * 8, 8 bits)
          }
      }
  }

  override def build(pipeline: MemPipeline): Unit = pipeline plug new Area {
    // retire
    val commitStore = pipeline.globalService(classOf[CommitPlugin]).commitStore
    val retireFall = !queue(0).retired +: (for (i <- 1 until depth)
      yield queue(i - 1).retired && !queue(i).retired)
    for (j <- 0 until depth) {
      when(commitStore && retireFall(j)) {
        // 定位匹配，则将槽retire
        queue(j).retired := True
        queueNext(j).retired := True
      }
    }

    // 入队逻辑
    val validFall = !queue(0).valid +: (for (i <- 1 until depth)
      yield queue(i - 1).valid && !queue(i).valid)
    queueIO.pushPort.ready := !queue.last.valid
    for (j <- 0 until depth) {
      when(queueIO.pushPort.valid && validFall(j)) {
        // 定位匹配，则将槽入队
        queue(j).push(queueIO.pushPort.payload)
        queueNext(j).push(queueIO.pushPort.payload)
      }
    }

    val flush = pipeline.globalService(classOf[CommitPlugin]).regFlush
    when(flush) {
      for (i <- 0 until depth) when(!queueNext(i).payload.retired) {
        queue(i).valid := False
        queueNext(i).valid := False
      }
    }

    // 压缩逻辑
    for (i <- 0 until depth) {
      when(queueIO.popPort.fire) {
        if (i + 1 < depth) queue(i) := queueNext(i + 1)
        else queue(i).valid := False
      }
    }
  }
}
