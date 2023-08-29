package NOP.pipeline.core

import spinal.core._
import spinal.lib._
import NOP.builder._

import scala.collection.mutable
import NOP._
import NOP.utils._
import NOP.constants.enum._
import NOP.pipeline.core._
import NOP.pipeline.priviledge.InterruptHandlerPlugin

class Timer64Plugin extends Plugin[MyCPUCore] {

  val timer64 = RegInit(U(0x0, 64 bits))

  val highBits = out UInt (32 bits)
  val lowBits = out UInt (32 bits)
  val counterId = out UInt (32 bits)

  override def build(pipeline: MyCPUCore): Unit = {

    val InterruptHandlerPlugin = pipeline.service(classOf[InterruptHandlerPlugin])

    counterId := InterruptHandlerPlugin.TID_TID
    highBits := timer64(63 downto 32)
    lowBits := timer64(31 downto 0)
    timer64 := timer64 + 1
  }
}
