package NOP.pipeline.priviledge

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.LoongArch
import NOP.pipeline.core._
import NOP.pipeline.fetch._
import NOP.pipeline._

class ExceptionHandlerPlugin extends Plugin[MyCPUCore] {
  var intHandler: InterruptHandlerPlugin = null

  // ! Registers
  // ! CRMD
  val CRMD_PLV = RegInit(B(0x0, 2 bits))
  val CRMD_IE = RegInit(False)
  val CRMD_DA = RegInit(True) // Direct access
  val CRMD_PG = RegInit(False) // 映射地址翻译模式
  val CRMD_DATF = RegInit(B(0x0, 2 bits))
  val CRMD_DATM = RegInit(B(0x0, 2 bits))

  // ! PRMD
  val PRMD_PPLV = RegInit(B(0x0, 2 bits))
  val PRMD_PIE = RegInit(False)

  // ! ECFG
  val ECFG_LIE_0 = RegInit(B(0x0, 10 bits))
  val ECFG_LIE_10 = RegInit(B(0x0, 1 bits))
  val ECFG_LIE_11 = RegInit(B(0x0, 2 bits))

  // ! ESTAT
  val ESTAT_IS_0 = out(RegInit(B(0x0, 2 bits)))
//  val ESTAT_IS_2 = RegInit(B(0x0, 8 bits))
  val ESTAT_IS_2 = out(RegInit(B(0x0, 8 bits)))
  val ESTAT_IS_11 = out(RegInit(B(0x0, 1 bits)))
//  val ESTAT_IS_12 = RegInit(B(0x0, 1 bits))
  val ESTAT_IS_12 = out(B(0x0, 1 bits))
  val ESTAT_ECODE = RegInit(B(0x0, 6 bits))
  val ESTAT_ESUBCODE = RegInit(B(0x0, 9 bits))

  // ! ERA
  val ERA_PC = RegInit(B(0x0, 32 bits))

  // ! BADV
  val BADV_VADDR = RegInit(B(0x0, 32 bits))

  // ! EENTRY
  val EENTRY_VA = RegInit(B(0x0, 26 bits))
  val EENTRY_PC = EENTRY_VA ## B(0x0, 6 bits)

  // ! SAVE0~3
  val SAVE0_DATA = RegInit(B(0x0, 32 bits))
  val SAVE1_DATA = RegInit(B(0x0, 32 bits))
  val SAVE2_DATA = RegInit(B(0x0, 32 bits))
  val SAVE3_DATA = RegInit(B(0x0, 32 bits))

  // ! LLBCTL
  val LLBCTL_LLBIT = out(RegInit(False))
  val LLBCTL_KLO = RegInit(False)

  override def setup(pipeline: MyCPUCore): Unit = {
    import LoongArch.CSRAddress
    val CSRMan = pipeline.service(classOf[CSRPlugin])
    intHandler = pipeline.service(classOf[InterruptHandlerPlugin])

    // !! CRMD
    CSRMan.rw(
      CSRAddress.CRMD,
      0 -> CRMD_PLV,
      2 -> CRMD_IE,
      3 -> CRMD_DA,
      4 -> CRMD_PG,
      5 -> CRMD_DATF,
      7 -> CRMD_DATM
    )
    CSRMan.r0(CSRAddress.CRMD, 9, (31 downto 9).length bits)

    // ! PRMD
    CSRMan.rw(
      CSRAddress.PRMD,
      0 -> PRMD_PPLV,
      2 -> PRMD_PIE
    )
    CSRMan.r0(CSRAddress.PRMD, 3, (31 downto 3).length bits)

    // ! EUEN
    CSRMan.r0(CSRAddress.EUEN, 0, 32 bits)

    // ! ECFG
    CSRMan.rw(
      CSRAddress.ECFG,
      0 -> ECFG_LIE_0,
      10 -> ECFG_LIE_10,
      11 -> ECFG_LIE_11
    )
    CSRMan.r0(CSRAddress.ECFG, 13, (31 downto 13).length bits)

    // ! ESTAT
    CSRMan.rw(CSRAddress.ESTAT, 0 -> ESTAT_IS_0)
    CSRMan.r(
      CSRAddress.ESTAT,
      2 -> ESTAT_IS_2,
      11 -> ESTAT_IS_11,
      12 -> ESTAT_IS_12,
      16 -> ESTAT_ECODE,
      22 -> ESTAT_ESUBCODE
    )
    CSRMan.r0(CSRAddress.ESTAT, 10, 1 bits)
    CSRMan.r0(CSRAddress.ESTAT, 13, 3 bits)
    CSRMan.r0(CSRAddress.ESTAT, 31, 1 bits)

    // ! ERA
    CSRMan.rw(CSRAddress.ERA, 0 -> ERA_PC)

    // ! BADV
    CSRMan.rw(CSRAddress.BADV, 0 -> BADV_VADDR)

    // ! EENTRY
    CSRMan.rw(CSRAddress.EENTRY, 6 -> EENTRY_VA)
    CSRMan.r0(CSRAddress.EENTRY, 0, 6 bits)

    // ! CPUID
    CSRMan.r0(CSRAddress.CPUID, 0, 32 bits)

    // ! SAVE0 ~ 3
    CSRMan.rw(CSRAddress.SAVE0, 0 -> SAVE0_DATA)
    CSRMan.rw(CSRAddress.SAVE1, 0 -> SAVE1_DATA)
    CSRMan.rw(CSRAddress.SAVE2, 0 -> SAVE2_DATA)
    CSRMan.rw(CSRAddress.SAVE3, 0 -> SAVE3_DATA)

    // ! LLBCTL
    CSRMan.rw(
      CSRAddress.LLBCTL,
      2 -> LLBCTL_KLO
    )
    CSRMan.r(CSRAddress.LLBCTL, 0 -> LLBCTL_LLBIT)
    CSRMan.w1(CSRAddress.LLBCTL, 1) {
      LLBCTL_LLBIT := False
    }

  }

  override def build(pipeline: MyCPUCore): Unit = pipeline plug new Area {
    import LoongArch.CSRAddress
    import LoongArch.ExceptionCode

    val jumpInterface = pipeline.fetchPipeline.service(classOf[ProgramCounterPlugin]).backendJumpInterface
    val excCommit = pipeline.service(classOf[CommitPlugin])
    val MMUPlugin = pipeline.service(classOf[MMUPlugin])
    val exceptPayload = excCommit.except
    val exceptErtn = excCommit.ertn
    val exceptEpc = excCommit.epc

    val takeInt = intHandler.intPending
    val eentry = UWord()
    eentry := EENTRY_PC.asUInt
    when(!takeInt && exceptPayload.payload.isTLBRefill) {
      eentry := MMUPlugin.TLBRENTRY_PA.asUInt @@ U(0x0, 6 bits)
    }

    when(exceptPayload.valid) {
      jumpInterface.valid := True
      jumpInterface.payload := eentry

      when(takeInt) {
        // Interrupt
        ESTAT_ECODE := B(LoongArch.ExceptionCode.INT.ecode, 6 bits)
        ESTAT_ESUBCODE := B(LoongArch.ExceptionCode.INT.esubcode, 9 bits)
      } otherwise {
        // Exception
        when(exceptPayload.isTLBRefill) {
          CRMD_DA := True
          CRMD_PG := False
        }
        // ESTAT
        ESTAT_ECODE := exceptPayload.payload.code
        ESTAT_ESUBCODE := exceptPayload.payload.subcode

        // BADV
        val BadVUpdateCases = Seq(
          LoongArch.ExceptionCode.TLBR,
          LoongArch.ExceptionCode.ADEF,
          LoongArch.ExceptionCode.ADEM,
          LoongArch.ExceptionCode.ALE,
          LoongArch.ExceptionCode.PIL,
          LoongArch.ExceptionCode.PIS,
          LoongArch.ExceptionCode.PIF,
          LoongArch.ExceptionCode.PME,
          LoongArch.ExceptionCode.PPI
        )
        when(
          BadVUpdateCases
            .map({ excCase =>
              exceptPayload.payload.code === excCase.ecode &&
              exceptPayload.payload.subcode === excCase.esubcode
            })
            .orR
        ) {
          BADV_VADDR := exceptPayload.payload.badVA.asBits
        }

        // TLBEHI
        val TLBEHIUpdateCases = Seq(
          LoongArch.ExceptionCode.TLBR,
          LoongArch.ExceptionCode.PIF,
          LoongArch.ExceptionCode.PPI,
          LoongArch.ExceptionCode.PME,
          LoongArch.ExceptionCode.PIS,
          LoongArch.ExceptionCode.PIL
        )
        when(
          BadVUpdateCases
            .map({ excCase =>
              exceptPayload.payload.code === excCase.ecode &&
              exceptPayload.payload.subcode === excCase.esubcode
            })
            .orR
        ) {
          MMUPlugin.TLBEHI_VPPN := exceptPayload.payload.badVA(31 downto 13).asBits
        }

      }

      PRMD_PPLV := CRMD_PLV
      PRMD_PIE := CRMD_IE
      CRMD_PLV := 0x0
      CRMD_IE := False

      ERA_PC := exceptEpc.asBits

    } elsewhen (excCommit.ertn) {

      when(LLBCTL_KLO) {
        LLBCTL_KLO := False
      } otherwise {
        LLBCTL_LLBIT := False
      }

      jumpInterface.valid := True
      jumpInterface.payload := ERA_PC.asUInt

      CRMD_PLV := PRMD_PPLV
      CRMD_IE := PRMD_PIE
      when(ESTAT_ECODE === B(0x3f, 6 bits)) {
        CRMD_DA := False
        CRMD_PG := True
      }
    }
  }

}
