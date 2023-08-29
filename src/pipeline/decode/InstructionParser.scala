package NOP.pipeline.decode

import spinal.core._
import NOP.constants.enum._

case class InstructionParser(val inst: Bits) extends Bundle {

  def rd = inst(4 downto 0).asUInt
  def rj = inst(9 downto 5).asUInt
  def rk = inst(14 downto 10).asUInt

  def sa = inst(14 downto 10).asUInt

  def imm12 = inst(21 downto 10).asUInt
  def signExtendImm12 = imm12.asSInt.resize(32 bits).asUInt
  def zeroExtendImm12 = imm12.resize(32 bits)

  def imm20 = inst(24 downto 5).asUInt
  def signExtendImm20_2 = (imm20 @@ U(0, 2 bits)).asSInt.resize(32 bits).asUInt
  def signExtendImm20_12 = imm20 @@ U(0, 12 bits)

  def csrAddr = inst(23 downto 10).asUInt

}
