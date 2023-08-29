package NOP.constants.enum

import spinal.core._
import spinal.lib._

import spinal.core.SpinalEnum

object MemOperationType extends SpinalEnum {
  val FETCH, LOAD, STORE = newElement()
}
