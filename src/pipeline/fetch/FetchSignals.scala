package NOP.pipeline.fetch

import spinal.core._
import spinal.lib._
import NOP.utils._
import NOP._
import NOP.pipeline.core._

final case class FetchPacket(fetchWidth: Int) extends Bundle {
  val pc = UWord()
  val insts = Vec(Flow(BWord()), fetchWidth)
  // 优化宽度，前端 exception 不需要 bad va，永远是 pc
  val except = Flow(ExceptionPayloadBundle(false))
}

case class BranchStatusPayload() extends Bundle {
  val target = UInt(30 bits)
  val isCall = Bool()
  val isReturn = Bool()
}

case class BranchTableEntry(config: BTBConfig) extends Bundle {
  val tag = UInt(config.tagRange.size bits)
  val statusBundle = BranchStatusPayload()
}
