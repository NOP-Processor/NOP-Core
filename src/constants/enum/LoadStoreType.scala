package NOP.constants.enum

import spinal.core._
import spinal.lib._
import spinal.core.SpinalEnum

object LoadStoreType extends SpinalEnum {
  val BYTE, HALF, WORD, BYTE_U, HALF_U = newElement()
  val CACOP = newElement()
  val PRELD = newElement()

  def toAxiSize(lsType: SpinalEnumCraft[LoadStoreType.type]): UInt = {
    val size = UInt(3 bits)
    switch(lsType) {
      is(BYTE, BYTE_U) {
        size := 0
      }
      is(HALF, HALF_U) {
        size := 1
      }
      default {
        size := 2
      }
    }
    size
  }
}
