package NOP.pipeline.core

import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.`enum`.LoadStoreType
import NOP.pipeline._

// Exception
final case class ExceptionPayloadBundle(genBadVA: Boolean) extends Bundle {
  val code = Bits(6 bits)
  val subcode = Bits(9 bits)
  val badVA = genBadVA generate UWord()
  val isTLBRefill = Bool()
}

// RegFile
final case class RegFileMappingEntryBundle(config: RegFileConfig) extends Bundle {
  val addr = UInt(config.arfAddrWidth bits)
  val prevAddr = UInt(config.prfAddrWidth bits)
  val prfAddr = UInt(config.prfAddrWidth bits)
}

/** ROB表项。
  *
  * ROB主要关心一条指令的执行状态，和需要提交给各个模块的提交信息。
  */
final case class ROBEntryBundle(config: MyCPUConfig, fullState: Boolean = true) extends Bundle {
  val info = ROBEntryInfoBundle(config)
  val state = ROBEntryStateBundle(config.regFile, fullState)
}

/** Info信息应当用于提交，不需要被writeback修改。
  *
  * @param config
  */
final case class ROBEntryInfoBundle(config: MyCPUConfig) extends Bundle {
  // ROB不需要完整指令信息
  val uop = ROBMicroOp(config)
  // 似乎只需要rename的写口？用于提交ARF状态
  val rename = ROBRenameRecordBundle(config.regFile)
  // frontend exception的badVA是pc
  val frontendExc = Bool
}

/** State信息关心指令当前执行状态，通常writeback会改变这些状态。
  *
  * @param config
  */
final case class ROBEntryStateBundle(config: RegFileConfig, full: Boolean = true) extends Bundle {
  // 完成则代表这条指令已经可以提交
  val complete = Bool
  // 完整异常信息
  val except = Flow(ExceptionPayloadBundle(full))
  // 分支预测恢复信息
  val mispredict = Bool
  val actualTaken = Bool

  // When full is true
  // LSU检测到uncached区段，需要提交时操作
  val lsuUncached = full generate Bool()
  // INT执行结果
  val intResult = full generate UWord()

  // TODO: [NOP] DiffTest Bundle. Remove this in the future.
  val isCount = full generate Bool()
  val count64ReadValue = full generate UInt(64 bits)
  val csrRstat = full generate Bool()
  val csrRdata = full generate UWord()
  val isLoad = full generate Bool()
  val isStore = full generate Bool()
  val isLL = full generate Bool()
  val isSC = full generate Bool()
  val lsType = full generate LoadStoreType()
  val vAddr = full generate UWord()
  val pAddr = full generate UWord()
  val storeData = full generate UWord()
  val myPC = full generate UWord()
}

// Subset for ROBEntryState
final case class ROBStateCompletePortBundle(config: MyCPUConfig) extends Bundle {
  val robIdx = UInt(config.rob.robAddressWidth bits)
  val complete = Bool
}

final case class ROBStateLSUPortBundle(config: MyCPUConfig) extends Bundle {
  val robIdx = UInt(config.rob.robAddressWidth bits)
  val except = Flow(ExceptionPayloadBundle(true))
  val lsuUncached = Bool
  val intResult = UWord()
  // TODO: [NOP] DiffTest Bundle. Remove this in the future.
  val isLoad = Bool()
  val isStore = Bool()
  val isLL = Bool()
  val isSC = Bool()
  val lsType = LoadStoreType()
  val vAddr = UWord()
  val pAddr = UWord()
  val storeData = UWord()
  val myPC = UWord()
}

final case class ROBStateALUPortBundle(config: MyCPUConfig) extends Bundle {
  val robIdx = UInt(config.rob.robAddressWidth bits)
  val except = Flow(ExceptionPayloadBundle(false))

  // TODO: [NOP] DiffTest Bundle. Remove this in the future.
  val intResult = UWord()
  val isCount = Bool()
  val count64ReadValue = UInt(64 bits)
  val csrRstat = Bool()
  val csrRdata = UWord()
  val myPC = UWord()
}

final case class ROBStateBRUPortBundle(config: MyCPUConfig) extends Bundle {
  val robIdx = UInt(config.rob.robAddressWidth bits)
  val except = Flow(ExceptionPayloadBundle(false))
  val mispredict = Bool
  val intResult = UWord()
  val actualTaken = Bool
}
