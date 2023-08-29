package NOP.pipeline.fetch

import NOP.builder._
import NOP.utils._
import NOP.constants._
import NOP._
import NOP.constants.`enum`.MemOperationType
import NOP.pipeline._
import NOP.pipeline.core.ExceptionMuxPlugin
import NOP.pipeline.priviledge.{ExceptionHandlerPlugin, MMUPlugin}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._
import NOP.pipeline.priviledge._

class InstAddrTranslatePlugin() extends Plugin[FetchPipeline] {
  val PIF = Bool()
  val PPI = Bool()
  val ADEF = Bool()
  val TLBR = Bool()
  val badVaddr = UInt(32 bits)
  val badVaddr2 = UInt(32 bits)

  override def setup(pipeline: FetchPipeline): Unit = {
    import LoongArch.ExceptionCode
    val iExec = pipeline.service(classOf[ExceptionMuxPlugin[FetchPipeline]])
    iExec.addExceptionSource(pipeline.IF1, ExceptionCode.ADEF, ADEF, badVaddr, priority = 100)
    iExec.addExceptionSource(pipeline.IF2, ExceptionCode.TLBR, TLBR, badVaddr2, priority = 90)
    iExec.addExceptionSource(pipeline.IF2, ExceptionCode.PIF, PIF, badVaddr2, priority = 80)
    iExec.addExceptionSource(pipeline.IF2, ExceptionCode.PPI, PPI, badVaddr2, priority = 70)
  }

  override def build(pipeline: FetchPipeline): Unit = {
    val mmu = pipeline.globalService(classOf[MMUPlugin])
    val excHandler = pipeline.globalService(classOf[ExceptionHandlerPlugin])

    pipeline.IF1 plug new Area {
      import pipeline.IF1._
      import pipeline.signals._

      // fetch packet不跨行，更不跨页
      val virtPC = input(PC)
      badVaddr := input(PC)
      ADEF := (input(PC)(0) || input(PC)(1))

      val directTranslateResult = mmu.directTranslate(virtPC, MemOperationType.FETCH)
      val tlbTranslateResult = mmu.tlbTranslate(virtPC, MemOperationType.FETCH)
      val savedCSR = TranslateCSRBundle()
      savedCSR.CRMD_DA := excHandler.CRMD_DA
      savedCSR.CRMD_PG := excHandler.CRMD_PG
      savedCSR.CRMD_DATF := excHandler.CRMD_DATF
      savedCSR.CRMD_DATM := excHandler.CRMD_DATM

      insert(DIRECT_TRANSLATE_RESULT) := directTranslateResult.resultBundle
      insert(TLB_TRANSLATE_RESULT) := tlbTranslateResult.resultBundle
      insert(TRANSLATE_SAVED_CSR) := savedCSR
    }

    pipeline.IF2 plug new Area {
      import pipeline.IF2._
      import pipeline.signals._

      val virtPC = input(PC)
      badVaddr2 := input(PC)

      val physPC = insert(PC_PHYSICAL)
      val pcCached = insert(ADDRESS_CACHED)
      val tlbRefill = insert(IS_TLB_REFILL)

      val translateResult = mmu.translate(
        virtPC,
        MemOperationType.FETCH,
        input(DIRECT_TRANSLATE_RESULT),
        input(TLB_TRANSLATE_RESULT),
        input(TRANSLATE_SAVED_CSR)
      )
      PIF := translateResult.resultExceptionBundle.raisePIF
      PPI := translateResult.resultExceptionBundle.raisePPI
      TLBR := translateResult.resultExceptionBundle.raiseTLBR

      physPC := translateResult.resultPhysAddr
      pcCached := translateResult.resultCached
      tlbRefill := TLBR

    }
  }
}
