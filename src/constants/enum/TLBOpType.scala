package NOP.constants.enum

import spinal.core.SpinalEnum

object TLBOpType extends SpinalEnum {
  val NONE, TLBSRCH, TLBRD, TLBWR, TLBFILL = newElement()
  val INVTLB1, INVTLB2, INVTLB3, INVTLB4, INVTLB5, INVTLB6 = newElement()
}
