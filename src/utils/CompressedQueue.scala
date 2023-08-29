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
  * 1. 不压缩时自身状态转移。同步至stateNext。
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
abstract class CompressedQueue[T <: IssueSlot](
    issConfig: IssueConfig,
    val decodeWidth: Int,
    val slotType: HardType[T]
) extends Plugin[MyCPUCore] {
  val issueWidth = issConfig.issueWidth
  val depth = issConfig.depth

  val busyAddrs: Vec[UInt] // For overwritten in subclasses
  var busyRsps: Vec[Bool] = null // Read from PRF
  def fuMatch(uop: MicroOp): Bool // For overwritten in subclasses

  private val grantPorts = mutable.ArrayBuffer[(Seq[Bool], Vec[Bool])]()
  def grantPort(reqs: Seq[Bool]) = {
    val grants = Vec(Bool, reqs.size)
    grantPorts += (reqs -> grants)
    grants
  }

  val queue = Vec(RegFlow(slotType()), depth) // 做槽移动
  val queueNext = CombInit(queue) // 标志对应槽的下一个值（不考虑压缩）  // 1. 不压缩时自身状态转移。同步至stateNext。

  val issueMask = Bits(depth bits)
  val issueFire = True // issue整体使能
  val queueFlush = False // 清除整个IQ

  val queueIO = new Area {
    val pushPorts = Vec(Stream(slotType), decodeWidth)
  }

  // 2. 入队时覆写为初态。同步至stateNext。
  def genEnqueueLogic() = {
    val validFall = !queue(0).valid +: (for (i <- 1 until depth)
      yield queue(i - 1).valid && !queue(i).valid) // the length of current valid depth
    // 入队逻辑
    for (i <- 0 until decodeWidth) {
      // 0口ready当且仅当depth-1是空的
      queueIO.pushPorts(i).ready := !queue(depth - i - 1).valid
      for (j <- i until depth) {
        when(queueIO.pushPorts(i).valid && validFall(j - i)) {
          // 定位匹配，则将槽入队
          queue(j).push(queueIO.pushPorts(i).payload) // Flow.push, set valid to True and payload to that
          queueNext(j).push(queueIO.pushPorts(i).payload)
        }
      }
    }
  }

  // 3. 压缩时通过stateNext同步状态转移。
  def genCompressLogic() = {
    // 压缩逻辑
    val popCounts = Vec(UInt(log2Up(issueWidth + 1) bits), depth) // sized depth, prefix sum of issueMask
    // pop等价于当前valid，下个周期不valid（被抽走了）
    popCounts(0) := (issueFire && issueMask(0)).asUInt.resized
    for (i <- 0 to depth - 2) {
      popCounts(i + 1) := Mux(
        issueFire && issueMask(i + 1),
        popCounts(i) + 1,
        popCounts(i)
      )
    }
    for (i <- 0 until depth) {
      for (j <- 1 to issueWidth) {
        // 后覆盖前。考察[0, i+j-1]的清除情况。例如0被清除，则0选择1，但01都被清除，则0选择2...
        if (i + j < depth) when(popCounts(i + j - 1) === j)(queue(i) := queueNext(i + j))
        else when(popCounts.last === j)(queue(i).valid := False)
      }
    }
  }

  // 4. flush时直接清除所有valid。直接写。最高优先级，不需要stateNext同步。
  def genFlushLogic() = when(queueFlush) {
    // flush最高优先级
    for (i <- 0 until depth) {
      queue(i).valid := False
    }
  }

  def genIssueSelect() = {
    // issue选择
    require(grantPorts.size <= issueWidth)

    // Grant only one slot for each FU
    if (grantPorts.nonEmpty) grantPorts(0)._2 := OHMasking.first(grantPorts(0)._1)

    for (i <- 1 until grantPorts.size) {
      val exeMask = grantPorts.map(_._2).take(i).reduceBalancedTree { (l, r) =>
        (l.asBits | r.asBits).asBools
      }
      grantPorts(i)._2 := OHMasking.first(grantPorts(i)._1.zip(exeMask).map { case (b, m) =>
        b && !m // Mask out already granted slots
      })
    }
    issueMask := grantPorts.map(_._2.asBits).reduceBalancedTree(_ | _) // orReduce
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
