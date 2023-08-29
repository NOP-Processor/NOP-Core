package NOP.utils

import spinal.core._
import spinal.lib._
import scala.collection.mutable

import NOP.pipeline.core._
import NOP.pipeline._
import NOP.builder._
import NOP._

/** 压缩队列。
  *
  * 压缩队列按优先级从低到高：
  *
  *   1. 不压缩时自身状态转移。同步至stateNext。
  *
  * 2. 入队时覆写为初态。同步至stateNext。
  *
  * 3. 压缩时通过stateNext同步状态转移。
  *
  * 4. flush时直接清除所有valid。直接写。最高优先级，不需要stateNext同步。
  *
  * @param issConfig
  * @param decodeWidth
  * @param slotType
  */
abstract class CompressedFIFO[T <: IssueSlot](
    issConfig: IssueConfig,
    val decodeWidth: Int,
    val slotType: HardType[T]
) extends Plugin[MyCPUCore] {
  val issueWidth = issConfig.issueWidth
  val depth = issConfig.depth

  val busyAddrs: Vec[UInt]
  var busyRsps: Vec[Bool] = null
  def fuMatch(uop: MicroOp): Bool

  val queue = out(Vec(RegFlow(slotType()), depth)) // 做槽移动
  val queueNext = CombInit(queue) // 标志对应槽的下一个值（不考虑压缩）

  val issueReq = Bool
  val issueFire = True // issue整体使能
  val queueFlush = False // 清除整个IQ

  val queueIO = new Area {
    val pushPorts = Vec(Stream(slotType), decodeWidth)
  }

  def genEnqueueLogic() = {
    val validFall = !queue(0).valid +: (for (i <- 1 until depth)
      yield queue(i - 1).valid && !queue(i).valid)
    // 入队逻辑
    for (i <- 0 until decodeWidth) {
      // 0口ready当且仅当depth-1是空的
      queueIO.pushPorts(i).ready := !queue(depth - i - 1).valid
      for (j <- i until depth) {
        when(queueIO.pushPorts(i).valid && validFall(j - i)) {
          // 定位匹配，则将槽入队
          queue(j).push(queueIO.pushPorts(i).payload)
          queueNext(j).push(queueIO.pushPorts(i).payload)
        }
      }
    }
  }

  def genCompressLogic() = {
    // 压缩逻辑
    val popCount = issueReq && issueFire
    require(issueWidth == 1)
    for (i <- 0 until depth) {
      when(popCount) {
        if (i + 1 < depth) queue(i) := queueNext(i + 1)
        else queue(i).valid := False
      }
    }
  }

  def genFlushLogic() = when(queueFlush) {
    // flush最高优先级
    for (i <- 0 until depth) {
      queue(i).valid := False
    }
  }

  def genIssueSelect() = {
    // issue选择
  }

  def genGlobalWakeup(prf: PhysRegFilePlugin, rPorts: Int) = prf.clearBusys.foreach { port =>
    for (i <- 0 until depth) {
      // 远程唤醒（监听写busy广播）
      for (j <- 0 until rPorts)
        when(port.valid && port.payload === queue(i).payload.rRegs(j).payload) {
          queue(i).payload.rRegs(j).valid := True
          queueNext(i).payload.rRegs(j).valid := True
        }
    }
  }

  override def setup(pipeline: MyCPUCore): Unit = {
    val PRF = pipeline.service(classOf[PhysRegFilePlugin])
    busyRsps = Vec(busyAddrs.map(PRF.readBusy(_)))
  }
}
