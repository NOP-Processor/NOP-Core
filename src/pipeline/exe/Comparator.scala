package NOP.pipeline.exe

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.constants.enum._
import NOP.pipeline._
import NOP.utils._
import scala.collection.mutable.ArrayBuffer

class Comparator() extends Component {
  val io = new Bundle {
    val src1 = in(UWord())
    val src2 = in(UWord())
    val op = in(CompareOpType())
    val result = out(Bool)
  }

  import io._
  import CompareOpType._

  val eq = src1 === src2

  val lt = src1.asSInt < src2.asSInt
  val le = lt || eq
  val gt = !le
  val ge = !lt

  val ltu = src1 < src2
  val leu = ltu || eq
  val gtu = !leu
  val geu = !ltu

  val ltz = src1.msb
  val eqz = !src1.orR
  val lez = ltz || eqz
  val gtz = !lez
  val gez = !ltz

  val resultList = ArrayBuffer[(Any, Bool)](
    EQ -> eq,
    NE -> !eq,
    EQZ -> eqz,
    NEZ -> !eqz,
    GE -> ge,
    LT -> lt,
    LE -> le,
    GT -> gt,
    GEU -> geu,
    LTU -> ltu,
    LEU -> leu,
    GTU -> gtu,
    GEZ -> !ltz,
    GTZ -> !lez,
    LEZ -> lez,
    LTZ -> ltz
  )
  result := op.muxList(resultList)
}
