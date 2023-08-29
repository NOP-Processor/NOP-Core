package NOP.pipeline.fetch

import spinal.core._
import spinal.lib._

import NOP._
import NOP.utils._
import NOP.builder._
import NOP.pipeline._

final case class BranchPredictInfoBundle(withAddr: Boolean = true) extends Bundle {
  val predictBranch = Bool
  val predictTaken = Bool
  val predictAddr = withAddr generate UWord()
}

final case class PredictRecoverBundle(config: FrontendConfig) extends Bundle {
  val recoverTop = UInt(log2Up(config.btb.rasEntries) bits)
  val predictCounter = UInt(config.bpu.counterWidth bits)
  val ghr = UInt(config.bpu.historyWidth bits)
}

final case class PredictUpdateBundle(config: FrontendConfig) extends Bundle {
  // 原先predict信息
  val predInfo = BranchPredictInfoBundle(false)
  val predRecover = PredictRecoverBundle(config)
  // 实际branch计算信息
  val branchLike = Bool
  val isTaken = Bool
  val isRet = Bool
  val isCall = Bool
  val mispredict = Bool
  val pc = UWord()
  // 这里永远记录branch target，顺序target通过PC恢复
  val target = UWord()
}
