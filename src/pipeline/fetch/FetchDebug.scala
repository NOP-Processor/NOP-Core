package NOP.pipeline.fetch

import NOP.blackbox.mem._
import NOP.pipeline._
import NOP.builder._
import NOP.utils._
import NOP._
import NOP.constants.`enum`.{CacheOpType, CacheSelType}
import NOP.pipeline.core.{CommitPlugin, ExceptionMuxPlugin}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

class FetchDebugPlugin(config: MyCPUConfig) extends Plugin[FetchPipeline] {
  // New signals for debug
  val DIF1_valid = out Bool ()
  val DIF1_stuck = out Bool ()
  val DIF1_doRefetch = out Bool ()
  val DIF1_pc = out Bits (32 bits)
  val DIF1_nextpc = out Bits (32 bits)
  val DIF1_directTranslate_physAddr = out Bits (32 bits)
  val DIF1_directTranslate_enabled = out Bool ()
  val DIF1_tlbTranslate_physAddr = out Bits (32 bits)
  val DIF2_valid = out Bool ()
  val DIF2_stuck = out Bool ()
  val DIF2_pc = out Bits (32 bits)
  val DIF2_directTranslate_enabled = out Bool ()
  val DIF2_directTranslate_physAddr = out Bits (32 bits)
  val DIF2_tlbTranslate_physAddr = out Bits (32 bits)
  val DIF2_physAddr = out Bits (32 bits)
  val DIF2_fetchPacket_payload_valid = out Bits (config.frontend.fetchWidth bits)
  val DIF2_fetchPacket_payload_insts = out Vec (Bits(32 bits), config.frontend.fetchWidth)

  override def build(pipeline: FetchPipeline): Unit = {
    val ICachePlugin = pipeline.service(classOf[ICachePlugin])
    val programCounterPlugin = pipeline.service(classOf[ProgramCounterPlugin])

    DIF1_valid := pipeline.IF1.arbitration.isValid
    DIF1_stuck := pipeline.IF1.arbitration.isStuck
    DIF1_doRefetch := False
    DIF1_pc := pipeline.IF1.output(pipeline.signals.PC).asBits
    DIF1_nextpc := programCounterPlugin.nextPC.asBits
    DIF1_directTranslate_physAddr := pipeline.IF1
      .output(pipeline.signals.DIRECT_TRANSLATE_RESULT)
      .payload
      .physAddr
      .asBits
    DIF1_directTranslate_enabled := pipeline.IF1.output(pipeline.signals.DIRECT_TRANSLATE_RESULT).valid
    DIF1_tlbTranslate_physAddr := pipeline.IF1.output(pipeline.signals.TLB_TRANSLATE_RESULT).payload.physAddr.asBits
    DIF2_valid := pipeline.IF2.arbitration.isValid
    DIF2_stuck := pipeline.IF2.arbitration.isStuck
    DIF2_pc := pipeline.IF2.input(pipeline.signals.PC).asBits
    DIF2_directTranslate_enabled := pipeline.IF2.input(pipeline.signals.DIRECT_TRANSLATE_RESULT).valid
    DIF2_directTranslate_physAddr := pipeline.IF2
      .input(pipeline.signals.DIRECT_TRANSLATE_RESULT)
      .payload
      .physAddr
      .asBits
    DIF2_tlbTranslate_physAddr := pipeline.IF2.input(pipeline.signals.TLB_TRANSLATE_RESULT).payload.physAddr.asBits
    DIF2_physAddr := pipeline.IF2.output(pipeline.signals.PC_PHYSICAL).asBits
    DIF2_fetchPacket_payload_valid := pipeline.IF2
      .output(pipeline.signals.FETCH_PACKET)
      .insts
      .map { inst => inst.valid }
      .asBits()
    DIF2_fetchPacket_payload_insts := Vec(
      pipeline.IF2.output(pipeline.signals.FETCH_PACKET).insts.map { inst => inst.payload }
    )

  }

}
