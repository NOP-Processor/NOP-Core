package NOP.pipeline

import spinal.core._
import spinal.lib._

import NOP._
import NOP.utils._
import NOP.builder._
import NOP.pipeline._
import NOP.constants.enum._

class IssueSlot(config: MyCPUConfig, rPorts: Int) extends Bundle {
  protected val rfConfig = config.regFile
  // 套一个flow接wake up信号
  val rRegs = Vec(Flow(UInt(rfConfig.prfAddrWidth bits)), rPorts)
  // 用于寻址对应的rob表项，写相关信息
  val robIdx = UInt(config.rob.robAddressWidth bits)
}

final case class IntIssueSlot(config: MyCPUConfig) extends IssueSlot(config, 2) {
  // 这里也不需要完整的uop，但是和ROB需要的uop又不一样
  val uop = IntIQMicroOp()
  val wReg = UInt(rfConfig.prfAddrWidth bits)
}

final case class MulDivIssueSlot(config: MyCPUConfig) extends IssueSlot(config, 2) {
  val uop = MulDivIQMicroOp()
  val wReg = UInt(rfConfig.prfAddrWidth bits)
}

final case class MemIssueSlot(config: MyCPUConfig) extends IssueSlot(config, 2) {
  // 这里也不需要完整的uop，但是和ROB需要的uop又不一样
  val uop = MemIQMicroOp()
  val wReg = UInt(rfConfig.prfAddrWidth bits)
}
