package NOP.pipeline

import spinal.core._
import spinal.lib._
import NOP.utils._
import NOP._

final case class RenameRecordBundle(config: RegFileConfig) extends Bundle {
  val rRegs = Vec(UInt(config.prfAddrWidth bits), config.rPortsEachInst)
  val wReg = UInt(config.prfAddrWidth bits)
  val wPrevReg = UInt(config.prfAddrWidth bits)
}

final case class ROBRenameRecordBundle(config: RegFileConfig) extends Bundle {
  val wReg = UInt(config.prfAddrWidth bits)
  val wPrevReg = UInt(config.prfAddrWidth bits)
}

// And MicroOps. See DecodeMicroOP.scala
