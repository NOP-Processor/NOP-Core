package NOP.pipeline.core

import spinal.core._
import spinal.lib._

import NOP.builder._

class SpeculativeWakeupHandler extends Plugin[MyCPUCore] {
  val wakeupFailed = False
  val regWakeupFailed = RegNext(wakeupFailed, init = False)
  override def build(pipeline: MyCPUCore): Unit = {}
}
