package NOP.constants.enum

import spinal.core._
import spinal.lib._

import NOP.utils._
import spinal.core.SpinalEnum

object CacheSelType extends SpinalEnum {
  val None, ICache, DCache, SharedCache = newElement()
}

object CacheOpType extends SpinalEnum {
  val None = newElement()
  val StoreTag = newElement()
  val IndexInvalidate = newElement()
  val HitInvalidate = newElement()
}
