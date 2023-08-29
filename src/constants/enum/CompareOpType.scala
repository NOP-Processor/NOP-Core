package NOP.constants.enum

import spinal.core.Spinal
import spinal.core.SpinalEnum

object CompareOpType extends SpinalEnum {
  val EQ, NE = newElement()
  val EQZ, NEZ = newElement()
  val GE, LT, LE, GT = newElement()
  val GEU, LTU, LEU, GTU = newElement()
  val GEZ, LTZ, LEZ, GTZ = newElement()
}
