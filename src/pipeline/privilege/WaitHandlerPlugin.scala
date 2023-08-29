package NOP.pipeline.priviledge

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.LoongArch
import NOP.pipeline.core._
import NOP.pipeline.fetch._
import NOP.pipeline._

class WaitHandlerPlugin extends Plugin[MyCPUCore] {
  override def build(pipeline: MyCPUCore): Unit = pipeline plug new Area {
    val inLowPowerMode = RegInit(False)
    inLowPowerMode setWhen pipeline.service(classOf[CommitPlugin]).doWait
    inLowPowerMode clearWhen pipeline.service(classOf[InterruptHandlerPlugin]).ESTAT.orR
    pipeline.IF1.arbitration.haltByOther setWhen inLowPowerMode
  }
}
