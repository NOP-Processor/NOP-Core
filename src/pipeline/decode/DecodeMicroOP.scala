package NOP.pipeline

import spinal.core._
import spinal.lib._
import NOP.utils._
import NOP.pipeline.fetch._
import NOP.pipeline.core._
import NOP.builder._
import NOP._
import NOP.constants.enum._

object MicroOpSignals {
  // 用同样的名字构造stageable，这样便于decoder编写
  // 与 micro op 保持一样的顺序，便于对照

  // dispatch 信息
  object fuType extends Stageable(FUType())

  // REG r/w 信息
  object useRj extends Stageable(Bool)
  object useRk extends Stageable(Bool)
  object useRd extends Stageable(Bool)
  object isRegWrite extends Stageable(Bool) // doRegWrite = isRegWrite && Rd != 0
  object overrideRdToRA extends Stageable(Bool)
  object overrideRdToRj extends Stageable(Bool)

  // IMM
  object immExtendType extends Stageable(ExtendType())

  // ALU
  object aluOp extends Stageable(ALUOpType())

  // CSR
  object readCSR extends Stageable(Bool())

  // Timer64
  object readTimer64H extends Stageable(Bool())

  // LSU
  object lsType extends Stageable(LoadStoreType())
  object isLoad extends Stageable(Bool)
  object isStore extends Stageable(Bool)

  // BRU
  object cmpOp extends Stageable(CompareOpType())
  object isBranch extends Stageable(Bool)
  object isJump extends Stageable(Bool)
  object isJR extends Stageable(Bool)

  // MUL / DIV
  object signed extends Stageable(Bool)

  // Priviledge-related OP
  object isErtn extends Stageable(Bool())
  object isSyscall extends Stageable(Bool()) // In epilogue, assigned directly to exception.valid = True
  object isBreak extends Stageable(Bool()) // In epilogue, assigned directly to exception.valid = True
  object isBar extends Stageable(Bool())

  object tlbOp extends Stageable(TLBOpType())
  object operateCache extends Stageable(Bool())
  object isWait extends Stageable(Bool())
  object isLL extends Stageable(Bool())
  object isSC extends Stageable(Bool())

}

final case class MicroOp(config: MyCPUConfig) extends Bundle {
  val pc = UWord() // Assigned in Epilogue
  val inst = BWord() // Assigned in Epilogue

  // Branch prediction
  val predInfo = BranchPredictInfoBundle() // Assigned in Epilogue
  val predRecover = PredictRecoverBundle(config.frontend) // Assigned in Epilogue

  // dispatch信息
  val fuType = FUType() // Assigned in Main

  // REG r/w信息
  val useRj = Bool
  val useRk = Bool
  val useRd = Bool
  val wbAddr = UInt(config.regFile.arfAddrWidth bits) // Assigned in Epilogue
  val doRegWrite = Bool // Re-assigned in Epilogue

  // IMM
  val immExtendType = ExtendType()

  // ALU
  val aluOp = ALUOpType()

  // LSU
  val lsType = LoadStoreType()
  val isLoad = Bool
  val isStore = Bool

  // BRU
  val cmpOp = CompareOpType()
  val isBranch = Bool
  val isJump = Bool
  val isJR = Bool
  val branchLike = Bool // Assigned in Epilogue

  // CSR
  val writeCSR = Bool() // Assigned in Epilogue
  val readCSR = Bool()

  // Timer64
  val readTimer64L = Bool()
  val readTimer64H = Bool()
  val readTimer64ID = Bool()

  // TLB操作
  val tlbOp = TLBOpType()
  def operateTLB = tlbOp =/= TLBOpType.NONE

  // cache操作
  val operateCache = Bool

//   // WAIT指令
  val isWait = Bool
  val isLL = Bool()
  val isSC = Bool()

  // commit信息
  val uniqueRetire = Bool // Assigned in Epilogue

  // MUL / DIV
  val signed = Bool

  // Exception
  val except = Flow(ExceptionPayloadBundle(false)) // Assigned in Epilogue
  val isErtn = Bool // Assigned in Epilogue

  // control flow transfer
  val flushState = Bool // Assigned in Epilogue

  def needNotExecute = except.valid || fuType === FUType.NONE
}

final case class IntIQMicroOp() extends Bundle {
  // Meta information
  val pc = UInt(32 bits)
  val inst = Bits(32 bits)

  // Branch prediction
  val predInfo = BranchPredictInfoBundle()

  // dispatch信息
  val fuType = FUType()

  // REG r/w信息
  val useRj = Bool
  val useRk = Bool
  val useRd = Bool
  val doRegWrite = Bool

  // IMM
  val immExtendType = ExtendType()

  // ALU
  val aluOp = ALUOpType()

  // BRU信息
  val cmpOp = CompareOpType()
  val isBranch = Bool()
  val isJump = Bool()
  val isJR = Bool()
  def branchLike = isBranch || isJump || isJR

  // CSR
  val writeCSR = Bool()
  val readCSR = Bool()

  // Timer64
  val readTimer64L = Bool()
  val readTimer64H = Bool()
  val readTimer64ID = Bool()

  // TLB操作
  val tlbOp = TLBOpType()
  def operateTLB = tlbOp =/= TLBOpType.NONE
}

final case class MulDivIQMicroOp() extends Bundle {
  // dispatch信息
  val fuType = FUType()
  // REG r/w信息
  val doRegWrite = Bool()
  // If MULH, DIV or MOD is signed
  val signed = Bool()
}

final case class MemIQMicroOp() extends Bundle {
  // Meta information
  val pc = UInt(32 bits)
  val inst = Bits(32 bits)

  val doRegWrite = Bool()
  // LSU信息
  val lsType = LoadStoreType()
  val isLoad = Bool()
  val isStore = Bool()

  val isLL = Bool()
  val isSC = Bool()

  // 需要单独赋值
  val immField = Bits(32 bits)
  val cacheOp = CacheOpType() // CACOP or PRELD Cache OP
  val cacheSel = CacheSelType()
}

final case class ROBMicroOp(config: MyCPUConfig) extends Bundle {
  val pc = UWord()
  val inst = BWord()
  // Branch Prediction
  val predInfo = BranchPredictInfoBundle(false)
  val predRecover = PredictRecoverBundle(config.frontend)
  // Write registers
  val wbAddr = UInt(config.regFile.arfAddrWidth bits)
  val doRegWrite = Bool

  // IMM、ALU、LSU、BRU信息均与执行相关，仅部分保留

  // LSU信息
  val isLoad = Bool
  val isStore = Bool
  val lsType = LoadStoreType()

  // Timer64
  val readTimer64L = Bool()
  val readTimer64H = Bool()
  val readTimer64ID = Bool()

  // BRU信息
  val isBranch = Bool
  val isJump = Bool
  val isJR = Bool
  val branchLike = Bool

  // TLB操作
  val tlbOp = TLBOpType()
  def operateTLB = tlbOp =/= TLBOpType.NONE

  // cache操作
  val operateCache = Bool()

  // CSR
  val writeCSR = Bool() // Assigned in Epilogue
  val readCSR = Bool()

  // // WAIT指令
  val isWait = Bool()

  val isLL = Bool()
  val isSC = Bool()

  // commit信息
  val uniqueRetire = Bool()
  // // 异常被另行保存
  val isErtn = Bool()
  // 非branch的控制流转移
  val flushState = Bool()
}
