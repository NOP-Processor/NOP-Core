package NOP.pipeline.mem

import spinal.core._
import spinal.lib._
import NOP.utils._
import NOP.constants.enum._
import NOP.pipeline.core._
import NOP.builder._
import NOP.pipeline._
import NOP.pipeline.decode._
import NOP._
import NOP.pipeline.exe.MemIssueQueuePlugin
import NOP.pipeline.priviledge._

class AddressGenerationPlugin(config: MyCPUConfig) extends Plugin[MemPipeline] {

  val raisePIL = Bool()
  val raisePIS = Bool()
  val raisePME = Bool()
  val raisePPI = Bool()
  val raiseALE = False
  val raiseTLBR = Bool()

  override def setup(pipeline: MemPipeline): Unit = {
    import NOP.constants.LoongArch.ExceptionCode
    val iExec = pipeline.service(classOf[ExceptionMuxPlugin[MemPipeline]])

    iExec.addExceptionSource(pipeline.MEMADDR, ExceptionCode.ALE, raiseALE, priority = 130)
    iExec.addExceptionSource(pipeline.MEM1, ExceptionCode.TLBR, raiseTLBR, priority = 120)
    iExec.addExceptionSource(pipeline.MEM1, ExceptionCode.PIL, raisePIL, priority = 115)
    iExec.addExceptionSource(pipeline.MEM1, ExceptionCode.PIS, raisePIS, priority = 110)
    iExec.addExceptionSource(pipeline.MEM1, ExceptionCode.PPI, raisePPI, priority = 105)
    iExec.addExceptionSource(pipeline.MEM1, ExceptionCode.PME, raisePME, priority = 100)

  }

  override def build(pipeline: MemPipeline): Unit = {
    val excHandler = pipeline.globalService(classOf[ExceptionHandlerPlugin])
    pipeline.MEMADDR plug new Area {

      import pipeline.MEMADDR._
      import pipeline.signals._

      val issSlot = input(ISSUE_SLOT)
      val uop = issSlot.uop
      val virtAddr = input(MEMORY_ADDRESS)
      val accessType = uop.lsType
      val isLoad = uop.isLoad
      val excAsLoad = !uop.isStore
      val isStore = uop.isStore
      val byteEnable = insert(MEMORY_BE).assignDontCare()
      val memWData = output(MEMORY_WRITE_DATA).allowOverride
      val dataWord = input(MEMORY_WRITE_DATA)

      // byte enable调整为对齐访问的，但是addr后两位保留，load后处理好写一些
      // 因此addr后两位对于实际load/store要忽略掉
      switch(accessType) {
        import LoadStoreType._
        is(BYTE, BYTE_U, CACOP, PRELD) {
          byteEnable := virtAddr(1 downto 0).mux(
            0 -> B"4'b0001",
            1 -> B"4'b0010",
            2 -> B"4'b0100",
            3 -> B"4'b1000"
          )
          memWData := virtAddr(1 downto 0).muxList(Seq.tabulate(4) { i =>
            i -> (dataWord |<< (i * 8))
          })
        }
        is(HALF, HALF_U) {
          raiseALE.setWhen(virtAddr(0))
          byteEnable := Mux(virtAddr(1), B"4'b1100", B"4'b0011")
          memWData := Mux(virtAddr(1), dataWord |<< 16, dataWord)
        }
        is(WORD) {
          raiseALE.setWhen(virtAddr(1 downto 0) =/= 0)
          byteEnable := 0xf
          memWData := dataWord
        }
      }
      // unaligned load用WDATA传rt的读结果，不可改变；aligned load不使用WDATA
      when(isLoad)(memWData := input(MEMORY_WRITE_DATA))

      // translate
      val mmu = pipeline.globalService(classOf[MMUPlugin])
      val directTranslateResult =
        mmu.directTranslate(virtAddr, Mux(isStore, MemOperationType.STORE, MemOperationType.LOAD))
      val tlbTranslateResult = mmu.tlbTranslate(virtAddr, Mux(isStore, MemOperationType.STORE, MemOperationType.LOAD))
      val savedCSR = TranslateCSRBundle()
      savedCSR.CRMD_DA := excHandler.CRMD_DA
      savedCSR.CRMD_PG := excHandler.CRMD_PG
      savedCSR.CRMD_DATF := excHandler.CRMD_DATF
      savedCSR.CRMD_DATM := excHandler.CRMD_DATM

      insert(DIRECT_TRANSLATE_RESULT) := directTranslateResult.resultBundle
      insert(TLB_TRANSLATE_RESULT) := tlbTranslateResult.resultBundle
      insert(TRANSLATE_SAVED_CSR) := savedCSR
    }

    pipeline.MEM1 plug new Area {

      import pipeline.MEM1._
      import pipeline.signals._

      val issSlot = input(ISSUE_SLOT)
      val uop = issSlot.uop
      val virtAddr = input(MEMORY_ADDRESS)
      val accessType = uop.lsType
      val isLoad = uop.isLoad
      val excAsLoad = !uop.isStore
      val isStore = uop.isStore
      val physAddr = insert(MEMORY_ADDRESS_PHYSICAL)

      val MMU = pipeline.globalService(classOf[MMUPlugin])
      val translateResult = MMU.translate(
        virtAddr,
        Mux(isStore, MemOperationType.STORE, MemOperationType.LOAD),
        input(DIRECT_TRANSLATE_RESULT),
        input(TLB_TRANSLATE_RESULT),
        input(TRANSLATE_SAVED_CSR)
      )
      insert(ADDRESS_CACHED) := translateResult.resultBundle.cached
      physAddr := translateResult.resultBundle.physAddr
      insert(IS_TLB_REFILL) := translateResult.resultExceptionBundle.raiseTLBR

      raisePIL := translateResult.resultExceptionBundle.raisePIL
      raisePIS := translateResult.resultExceptionBundle.raisePIS
      raisePME := translateResult.resultExceptionBundle.raisePME
      raisePPI := translateResult.resultExceptionBundle.raisePPI
      raiseTLBR := translateResult.resultExceptionBundle.raiseTLBR
    }
  }
}
