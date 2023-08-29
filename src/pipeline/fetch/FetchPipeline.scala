package NOP.pipeline.fetch

import NOP._
import NOP.utils._
import NOP.builder._
import NOP.pipeline._
import NOP.pipeline.priviledge._

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

// Definition of Signals and Pipeline

class FetchSignals(config: MyCPUConfig) {
  private val fetchWidth = config.frontend.fetchWidth

  // Inner Usage
  object PC extends Stageable(UWord()) // ProgramCounterPlugin
  object NEXT_PC extends Stageable(UWord()) // ProgramCounterPlugin
  object PC_PHYSICAL extends Stageable(UWord()) // AddrTranslatePlugin
  object ADDRESS_CACHED extends Stageable(Bool())
  object IS_TLB_REFILL extends Stageable(Bool())

  // ADDR
  object DIRECT_TRANSLATE_RESULT extends Stageable(Flow(TranslateResultBundle()))
  object TLB_TRANSLATE_RESULT extends Stageable(Flow(TranslateResultBundle()))
  object TRANSLATE_SAVED_CSR extends Stageable(TranslateCSRBundle())

  // BTB & BPU & RAS
  object PREDICT_JUMP_FLAG extends Stageable(Bool())
  object PREDICT_JUMP_PAYLOAD extends Stageable(BranchStatusPayload())
  object PREDICT_JUMP_WAY extends Stageable(UInt(log2Up(config.frontend.fetchWidth) bits))
  object INSTRUCTION_MASK extends Stageable(Bits(fetchWidth bits))
  object BRANCH_MASK extends Stageable(Bits(fetchWidth bits))
  object TAKEN_MASK extends Stageable(Bits(fetchWidth bits))
  object PRED_COUNTER extends Stageable(Vec(UInt(config.frontend.bpu.counterWidth bits), fetchWidth))
  object GLOBAL_BRANCH_HISTORY extends Stageable(UInt(config.frontend.bpu.historyWidth bits))
  object PRIVATE_BRANCH_HISTORY extends Stageable(Vec(UInt(config.frontend.bpu.historyWidth bits), fetchWidth))
  object PREDICT_ADDR extends Stageable(UWord())
  object RECOVER_TOP extends Stageable(UInt(log2Up(config.frontend.btb.rasEntries) bits))

  // Outer Usage
  object FETCH_PACKET extends Stageable(FetchPacket(fetchWidth))
}

trait FetchPipeline extends Pipeline {
  type T = FetchPipeline
  val IF1: Stage = null
  val IF2: Stage = null
  val signals: FetchSignals
}
