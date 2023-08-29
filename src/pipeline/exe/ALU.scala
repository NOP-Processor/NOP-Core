package NOP.pipeline.exe

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.constants.enum._
import NOP.pipeline._
import NOP.utils._

// Combinatorial ALU
class ALU() extends Component {
  val io = new Bundle {
    val src1 = in(UWord())
    val src2 = in(UWord())
    val sa = in(UInt(5 bits))
    val op = in(ALUOpType())
    val result = out(UWord())
  }
  import io._

  switch(op) {
    import ALUOpType._
    is(ADD, LU12I, PCADDI, PCADDU12I) {
      result := src1 + src2
    }
    is(ADDU) {
      result := src1 + src2
    }
    is(SUB) {
      result := src1 - src2
    }
    is(SUBU) {
      result := src1 - src2
    }
    is(AND) {
      result := src1 & src2
    }
    is(OR) {
      result := src1 | src2
    }
    is(XOR) {
      result := src1 ^ src2
    }
    is(NOR) {
      result := ~(src1 | src2)
    }
    is(SLT) {
      result := (src1.asSInt < src2.asSInt).asUInt.resized
    }
    is(SLTU) {
      result := (src1 < src2).asUInt.resized
    }
    is(SLL) {
      result := src1 |<< sa
    }
    is(SRL) {
      result := src1 |>> sa
    }
    is(SRA) {
      result := (src1.asSInt >> sa).asUInt
    }

  }
}
