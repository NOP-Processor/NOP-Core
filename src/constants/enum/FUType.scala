package NOP.constants.enum

import spinal.core.SpinalEnum

/** Function unit type.
  */
object FUType extends SpinalEnum {
  val NONE = newElement()
  val ALU, CMP, CSR, TIMER, INVTLB = newElement()
  val MUL, MULH, DIV, MOD = newElement()
  val LSU = newElement()
}
