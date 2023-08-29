package NOP.pipeline

import NOP.builder._
import NOP.pipeline._
import NOP._

import spinal.core._
import spinal.lib._

class DecodeSignals(config: MyCPUConfig) {
  private val decodeWidth = config.decode.decodeWidth
  object DECODE_PACKET extends Stageable(Vec(Flow(MicroOp(config)), decodeWidth))
  object RENAME_RECORDS extends Stageable(Vec(RenameRecordBundle(config.regFile), decodeWidth))
  object ROB_INDEXES extends Stageable(Vec(UInt(config.rob.robAddressWidth bits), decodeWidth))
}

trait DecodePipeline extends Pipeline {
  type T = DecodePipeline
  val ID: Stage = null
  val RENAME: Stage = null
  val DISPATCH: Stage = null
  val signals: DecodeSignals
}
