package NOP.constants

import NOP._
import NOP.builder._
import NOP.constants.LoongArch.Bits
import NOP.utils._
import spinal.core._
import spinal.lib._

// LoongArch Instruction Set Architecture
object LoongArch {
  private def Rk(): String = "-----"
  private def Rj(): String = "-----"
  private def Rd(): String = "-----"
  private def Bits(len: Integer): String = "-" * len

  private def MakeMaskedLiteral(mask: String): MaskedLiteral = {
    // Replace spaces with nothing
    val replaced_mask = mask.replace(" ", "")
    assert(replaced_mask.length == 32)
    MaskedLiteral(replaced_mask)
  }

  // Instruction Masks
  val ADD = MakeMaskedLiteral("00000000000100000" + Rk() + Rj() + Rd())
  val SUB = MakeMaskedLiteral("00000000000100010" + Rk() + Rj() + Rd())
  val SLT = MakeMaskedLiteral("00000000000100100" + Rk() + Rj() + Rd())
  val SLTU = MakeMaskedLiteral("00000000000100101" + Rk() + Rj() + Rd())
  val NOR = MakeMaskedLiteral("00000000000101000" + Rk() + Rj() + Rd())
  val AND = MakeMaskedLiteral("00000000000101001" + Rk() + Rj() + Rd())
  val OR = MakeMaskedLiteral("00000000000101010" + Rk() + Rj() + Rd())
  val XOR = MakeMaskedLiteral("00000000000101011" + Rk() + Rj() + Rd())
  val SLL = MakeMaskedLiteral("00000000000101110" + Rk() + Rj() + Rd())
  val SRL = MakeMaskedLiteral("00000000000101111" + Rk() + Rj() + Rd())
  val SRA = MakeMaskedLiteral("00000000000110000" + Rk() + Rj() + Rd())

  val SLLI = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1" + Bits(5) + Rj() + Rd())
  val SRLI = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1" + Bits(5) + Rj() + Rd())
  val SRAI = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1" + Bits(5) + Rj() + Rd())

  val SLTI = MakeMaskedLiteral("0000001000" + Bits(12) + Rj() + Rd())
  val SLTUI = MakeMaskedLiteral("0000001001" + Bits(12) + Rj() + Rd())
  val ADDI = MakeMaskedLiteral("0000001010" + Bits(12) + Rj() + Rd())
  val ANDI = MakeMaskedLiteral("0000001101" + Bits(12) + Rj() + Rd())
  val ORI = MakeMaskedLiteral("0000001110" + Bits(12) + Rj() + Rd())
  val XORI = MakeMaskedLiteral("0000001111" + Bits(12) + Rj() + Rd())

  val LU12I = MakeMaskedLiteral("0 0 0 1 0 1 0" + Bits(20) + Rd())
  val PCADDI = MakeMaskedLiteral("0 0 0 1 1 0 0" + Bits(20) + Rd())
  val PCADDU12I = MakeMaskedLiteral("0 0 0 1 1 1 0" + Bits(20) + Rd())

  val JIRL = MakeMaskedLiteral("0 1 0 0 1 1" + Bits(16) + Rj() + Rd())
  val B = MakeMaskedLiteral("0 1 0 1 0 0" + Bits(16) + Bits(10))
  val BL = MakeMaskedLiteral("0 1 0 1 0 1" + Bits(16) + Bits(10))
  val BEQ = MakeMaskedLiteral("010110" + Bits(16) + Rj() + Rd())
  val BNE = MakeMaskedLiteral("010111" + Bits(16) + Rj() + Rd())
  val BLT = MakeMaskedLiteral("011000" + Bits(16) + Rj() + Rd())
  val BGE = MakeMaskedLiteral("011001" + Bits(16) + Rj() + Rd())
  val BLTU = MakeMaskedLiteral("011010" + Bits(16) + Rj() + Rd())
  val BGEU = MakeMaskedLiteral("011011" + Bits(16) + Rj() + Rd())

  val MUL = MakeMaskedLiteral("00000000000111000" + Rk() + Rj() + Rd())
  val MULH = MakeMaskedLiteral("00000000000111001" + Rk() + Rj() + Rd())
  val MULHU = MakeMaskedLiteral("00000000000111010" + Rk() + Rj() + Rd())
  val DIV = MakeMaskedLiteral("00000000001000000" + Rk() + Rj() + Rd())
  val MOD = MakeMaskedLiteral("00000000001000001" + Rk() + Rj() + Rd())
  val DIVU = MakeMaskedLiteral("00000000001000010" + Rk() + Rj() + Rd())
  val MODU = MakeMaskedLiteral("00000000001000011" + Rk() + Rj() + Rd())

  val LDB = MakeMaskedLiteral("0010100000" + Bits(12) + Rj() + Rd()) // 21 downto 10
  val LDBU = MakeMaskedLiteral("0010101000" + Bits(12) + Rj() + Rd())
  val LDH = MakeMaskedLiteral("0010100001" + Bits(12) + Rj() + Rd())
  val LDHU = MakeMaskedLiteral("0010101001" + Bits(12) + Rj() + Rd())
  val LDW = MakeMaskedLiteral("0010100010" + Bits(12) + Rj() + Rd())
  val STB = MakeMaskedLiteral("0010100100" + Bits(12) + Rj() + Rd())
  val STH = MakeMaskedLiteral("0010100101" + Bits(12) + Rj() + Rd())
  val STW = MakeMaskedLiteral("0010100110" + Bits(12) + Rj() + Rd())

  val ERTN = MakeMaskedLiteral("00000110010010000011100000000000")
  val BREAK = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0" + Bits(15))
  val SYSCALL = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 0" + Bits(15))
  val CSR = MakeMaskedLiteral("0 0 0 0 0 1 0 0" + Bits(14) + Rj() + Rd())
  val IDLE = MakeMaskedLiteral("0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 1" + Bits(15))

  val RDCNTVL = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0" + Rj() + Rd())
  val RDCNTVH = MakeMaskedLiteral("0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1" + "0 0 0 0 0" + Rd())

  val CACOP = MakeMaskedLiteral("0 0 0 0 0 1 1 0 0 0" + Bits(12) + Rj() + Bits(5)) // 4 downto 0
  val PRELD = MakeMaskedLiteral("0 0 1 0 1 0 1 0 1 1" + Bits(12) + Rj() + Bits(5)) // Not used :)
  val DBAR = MakeMaskedLiteral("0 0 1 1 1 0 0 0 0 1 1 1 0 0 1 0 0" + Bits(15))
  val IBAR = MakeMaskedLiteral("0 0 1 1 1 0 0 0 0 1 1 1 0 0 1 0 1" + Bits(15))

  val LL = MakeMaskedLiteral("0 0 1 0 0 0 0 0" + Bits(14) + Rj() + Rd())
  val SC = MakeMaskedLiteral("0 0 1 0 0 0 0 1" + Bits(14) + Rj() + Rd())

  val TLBSRCH = MakeMaskedLiteral("00000110010010000010100000000000")
  val TLBRD = MakeMaskedLiteral("00000110010010000010110000000000")
  val TLBWR = MakeMaskedLiteral("00000110010010000011000000000000")
  val TLBFILL = MakeMaskedLiteral("00000110010010000011010000000000")
  val INVTLB = MakeMaskedLiteral("0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 1 1" + Rk() + Rj() + Bits(5))

  object CSRAddress {
    val CRMD = 0x0
    val PRMD = 0x1
    val EUEN = 0x2
    val ECFG = 0x4
    val ESTAT = 0x5
    val ERA = 0x6
    val BADV = 0x7
    val EENTRY = 0xc
    val TLBIDX = 0x10
    val TLBEHI = 0x11
    val TLBELO0 = 0x12
    val TLBELO1 = 0x13
    val ASID = 0x18
    val PGDL = 0x19
    val PGDH = 0x1a
    val PGD = 0x1b
    val CPUID = 0x20
    val SAVE0 = 0x30
    val SAVE1 = 0x31
    val SAVE2 = 0x32
    val SAVE3 = 0x33
    val TID = 0x40
    val TCFG = 0x41
    val TVAL = 0x42
    val TICLR = 0x44
    val LLBCTL = 0x60
    val TLBRENTRY = 0x88
    val CTAG = 0x98
    val DMW0 = 0x180
    val DMW1 = 0x181
  }

  object ExceptionCode {
    case class Exception(ecode: Int, esubcode: Int) {}

    def ECode(x: Int) = x
    def EsubCode(x: Int) = x

    val INT = Exception(ECode(0x0), EsubCode(0))
    val PIL = Exception(ECode(0x1), EsubCode(0)) // Load 操作页无效
    val PIS = Exception(ECode(0x2), EsubCode(0)) // Store 操作页无效
    val PIF = Exception(ECode(0x3), EsubCode(0)) // 取指操作页无效
    val PME = Exception(ECode(0x4), EsubCode(0)) // 页修改例外
    val PPI = Exception(ECode(0x7), EsubCode(0)) // 页特权例外
    val ADEF = Exception(ECode(0x8), EsubCode(0)) // 地址错 (Fetch)
    val ADEM = Exception(ECode(0x8), EsubCode(1)) // 地址错 (Load/Store)
    val ALE = Exception(ECode(0x9), EsubCode(0)) // 地址不对齐例外
    val SYS = Exception(ECode(0xb), EsubCode(0)) // 系统调用例外
    val BRK = Exception(ECode(0xc), EsubCode(0)) // 断点例外
    val INE = Exception(ECode(0xd), EsubCode(0)) // 指令非法例外
    val IPE = Exception(ECode(0xe), EsubCode(0)) // 指令特权例外
    val FPD = Exception(ECode(0xf), EsubCode(0)) // 浮点例外, not implemented
    val FPE = Exception(ECode(0x12), EsubCode(1)) // 浮点例外, not implemented
    val TLBR = Exception(ECode(0x3f), EsubCode(0)) // TLB Refill
  }

}
