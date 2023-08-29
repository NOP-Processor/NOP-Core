package NOP.pipeline.exe

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.constants.enum._
import NOP.pipeline._
import NOP.utils._

class BRU extends Component {
  val io = new Bundle {
    // predicts
    val predictJump = in(Bool)
    val predictAddr = in(UWord())

    // actual
    val isBranch = in(Bool)
    val isJump = in(Bool) // B, BL
    val isJR = in(Bool) // JIRL

    val pc = in(UWord())
    val rj = in(UInt())
    val inst = in(Bits(32 bits))
    val condition = in(Bool)

    // outputs
    val mispredict = out(Bool)
    val actualTarget = out(UWord()) // without prediction, actual target
  }

  import io._
  val nextPC = pc + 4

  // BEQ, BNE, BLT, BGE, BLTU, BGEU
  val branchOffset = UWord()
  branchOffset := (inst(25 downto 10).asSInt @@ U(0, 2 bits)).resize(32 bits).asUInt

  // B or BL
  val jumpTarget = UWord()
  jumpTarget := pc + (inst(9 downto 0).asSInt @@ inst(25 downto 10).asSInt @@ U(0, 2 bits)).resize(32 bits).asUInt

  // JIRL
  val linkTarget = UWord()
  linkTarget := rj + (inst(25 downto 10).asUInt @@ U(0, 2 bits)).asSInt.resize(32 bits).asUInt

  when(isBranch) {
    actualTarget := pc + branchOffset
  } elsewhen (isJump) {
    actualTarget := jumpTarget
  } otherwise {
    actualTarget := linkTarget
  }

  when(isBranch || isJR || isJump) {
    mispredict := (predictJump ^ (condition || isJR || isJump)) || ((condition || isJR || isJump) && (actualTarget =/= predictAddr))
  } otherwise {
    mispredict := predictJump
  }
}
