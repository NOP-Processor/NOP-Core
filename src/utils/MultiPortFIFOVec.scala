package NOP.utils

import spinal.core._
import spinal.lib._

/** 多口同步FIFO，要求读写口等宽，但一次可以读写更少的量，不过必须在顺序的口上。
  *
  * 寄存器Vec实现，仅可用于小规模FIFO。好处是延迟仅为1 cycle，很方便插进流水线。
  *
  * 注意同步填充暂不支持。也就是说如果满了，即使这个周期要pop，也不能push，对时序好一些。
  *
  * @param dataType
  * @param depth
  * @param pushCount
  * @param popCount
  */
class MultiPortFIFOVec[T <: Data](
    dataType: HardType[T],
    depth: Int,
    pushPorts: Int,
    popPorts: Int,
    dataInit: Int => T = null,
    pushInit: Int = 0,
    popInit: Int = 0,
    initFull: Boolean = false
) extends Component {
  // pow2保证指针循环没有问题
  require(depth > 0 && isPow2(depth))
  val addressWidth = log2Up(depth)
  val dataVec =
    if (dataInit != null) Vec.tabulate(depth)({ i => RegInit(dataInit(i)) })
    else Reg(Vec(dataType, depth))
  def dataRead(addr: UInt) = dataVec(addr)
  def dataWrite(addr: UInt, value: T) = dataVec(addr) := value
  val pushPtr = RegInit(U(pushInit, addressWidth bits))
  val popPtr = RegInit(U(popInit, addressWidth bits))
  val pushEquals = (0 until depth).map { i => pushPtr === i }
  val popEquals = (0 until depth).map { i => popPtr === i }
  def dataRead(base: UInt, offset: Int) = {
    val result =
      dataVec.dataType().setCompositeName(dataVec, "readPort", weak = true).assignDontCare()
    dataVec.zipWithIndex.foreach { case (d, i) =>
      when(popEquals((i - offset + depth) % depth))(result := dataVec(i))
    }
    result
  }
  def dataWrite(base: UInt, offset: Int, value: T) = dataVec.zipWithIndex.foreach { case (d, i) =>
    when(pushEquals((i - offset + depth) % depth))(dataVec(i) := value)
  }
  val isRisingOccupancy = RegInit(Bool(initFull))
  val isEmpty = pushPtr === popPtr && !isRisingOccupancy
  val isFull = pushPtr === popPtr && isRisingOccupancy
  val io = new Bundle {

    /** push的valid，pop的ready，都要遵循连续性，从头开始的第一个0就表示了停止的位置，后面的1都会被忽略。
      */
    val push = Vec(slave(Stream(dataType)), pushPorts)
    val pop = Vec(master(Stream(dataType)), popPorts)
  }
  // push logic
  val maxPush = popPtr - pushPtr
  // 找到第一个非fire的
  val pushCount = PriorityMux(io.push.zipWithIndex.map { case (p, i) =>
    !p.fire -> U(i)
  } :+ (True -> U(pushPorts)))
  for (i <- 0 until pushPorts) {
    // 所有左侧均valid，且可以push
    val validTakeLeft = io.push.take(i + 1).map(_.valid).andR
    io.push(i).ready := isEmpty || i < maxPush
    when(io.push(i).ready && validTakeLeft)(dataWrite(pushPtr, i, io.push(i).payload))
  }
  pushPtr := pushPtr + pushCount

  // pop logic
  val maxPop = pushPtr - popPtr
  // 找到第一个非fire的
  val popCount = PriorityMux(io.pop.zipWithIndex.map { case (p, i) =>
    !p.fire -> U(i)
  } :+ (True -> U(popPorts)))
  for (i <- 0 until popPorts) {
    // 所有左侧均ready，且可以pop
    val readyTakeLeft = io.pop.take(i + 1).map(_.ready).andR
    io.pop(i).valid := isFull || i < maxPop
    io.pop(i).payload := dataRead(popPtr, i)
  }
  popPtr := popPtr + popCount

  when(pushCount =/= popCount)(isRisingOccupancy := pushCount > popCount)
}

class MultiPortFIFOVecOutReg[T <: Data](
    dataType: HardType[T],
    depth: Int,
    pushPorts: Int,
    popPorts: Int,
    dataInit: Int => T = null,
    pushInit: Int = 0,
    popInit: Int = 0,
    initFull: Boolean = false
) extends Component {
  // pow2保证指针循环没有问题
  require(depth > 0 && isPow2(depth))
  val addressWidth = log2Up(depth)
  val dataVec =
    if (dataInit != null) Vec.tabulate(depth)({ i => RegInit(dataInit(i)) })
    else Reg(Vec(dataType, depth))
  val pushPtr = RegInit(U(pushInit, addressWidth bits))
  val popPtr = RegInit(U(popInit, addressWidth bits))
  val pushEquals = (0 until depth).map { i => pushPtr === i }
  val popEquals = (0 until depth).map { i => popPtr === i }
  val isRisingOccupancy = RegInit(Bool(initFull))
  val isEmpty = pushPtr === popPtr && !isRisingOccupancy
  val isFull = pushPtr === popPtr && isRisingOccupancy
  val io = new Bundle {

    /** push的valid，pop的ready，都要遵循连续性，从头开始的第一个0就表示了停止的位置，后面的1都会被忽略。
      */
    val push = Vec(slave(Stream(dataType)), pushPorts)
    val pop = Vec(master(Stream(dataType)), popPorts)
  }

  // pop logic
  val maxPop = pushPtr - popPtr
  // 找到第一个非fire的
  val popCount = PriorityMux(io.pop.zipWithIndex.map { case (p, i) =>
    !p.fire -> U(i)
  } :+ (True -> U(popPorts)))
  for (i <- 0 until popPorts) {
    // 所有左侧均ready，且可以pop
    val readyTakeLeft = io.pop.take(i + 1).map(_.ready).andR
    io.pop(i).valid := isFull || i < maxPop
    io.pop(i).payload.setAsReg()
    io.pop(i).payload := dataVec(popPtr + popCount + i)
  }
  popPtr := popPtr + popCount

  // push logic
  val maxPush = popPtr - pushPtr
  // 找到第一个非fire的
  val pushCount = PriorityMux(io.push.zipWithIndex.map { case (p, i) =>
    !p.fire -> U(i)
  } :+ (True -> U(pushPorts)))
  for (i <- 0 until pushPorts) {
    // 所有左侧均valid，且可以push
    val validTakeLeft = io.push.take(i + 1).map(_.valid).andR
    io.push(i).ready := isEmpty || i < maxPush
    when(io.push(i).ready && validTakeLeft) {
      dataVec(pushPtr + i) := io.push(i).payload
      for (j <- i until popPorts) when(pushPtr + i === popPtr + popCount + j) {
        io.pop(j).payload := io.push(i).payload
      }
    }
  }
  pushPtr := pushPtr + pushCount

  when(pushCount =/= popCount)(isRisingOccupancy := pushCount > popCount)
}
