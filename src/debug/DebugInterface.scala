// Copied from Zencove::DebugInterface
// Used for debugging purposes
package NOP.debug

import spinal.core._
import spinal.lib._

class DebugRegFile() extends Bundle {
  val wen = Bits(4 bits)
  val wnum = UInt(5 bits)
  val wdata = Bits(32 bits)
}

class DebugWriteback() extends Bundle {
  val pc = UInt(32 bits)
  val rf = new DebugRegFile()
  val inst = Bits(32 bits)
}

class DebugInterface() extends Bundle {
  val wb = new DebugWriteback()
}
