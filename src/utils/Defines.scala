package NOP.utils

import spinal.core._

class UFixed(bitLen: Int) {
  def apply(): UInt = UInt(bitLen bits)
  def apply(value: Long): UInt = U(value, bitLen bits)
}

object UWord extends UFixed(32)
object RegAddr extends UFixed(5)

class BFixed(bitLen: Int) {
  def apply(): Bits = Bits(bitLen bits)
  def apply(value: Long): Bits = B(value, bitLen bits)
}

object BWord extends BFixed(32)
