package NOP.blackbox.execute

import spinal.core._
import spinal.lib._
import spinal.sim._

class Multiplier(dataWidth: Int = 32, name: String = "multiplier") extends BlackBox {
  setDefinitionName(name)
  noIoPrefix()
  val io = new Bundle {
    val CLK = in Bool ()
    val A = in UInt (dataWidth bits)
    val B = in UInt (dataWidth bits)
    val P = out UInt (dataWidth * 2 bits)
  }
  mapClockDomain(clock = io.CLK)

}
