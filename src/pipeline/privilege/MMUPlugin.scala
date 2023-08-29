package NOP.pipeline.priviledge

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.LoongArch
import NOP.constants.LoongArch.CSR
import NOP.constants.`enum`.{MemOperationType, TLBOpType}
import NOP.pipeline.core._
import NOP.pipeline.fetch._
import NOP.pipeline._

// ! Return Type Flow[TranslateResultBundle]
class TranslateResultExceptionBundle extends Bundle {
  val raisePIL = False // MEM
  val raisePIS = False // MEM
  val raisePIF = False // FETCH
  val raisePME = False // MEM
  val raisePPI = False // FETCH & MEM
  val raiseTLBR = False // FETCH & MEM
}

final case class TranslateResultBundle() extends Bundle {
  val physAddr = UInt(32 bits)
  val cached = Bool()
  val exception = new TranslateResultExceptionBundle
}

final case class TranslateCSRBundle() extends Bundle {
  val CRMD_DA = Bool()
  val CRMD_PG = Bool()
  val CRMD_DATF = Bits(2 bits)
  val CRMD_DATM = Bits(2 bits)
}

class MMUPlugin(config: MyCPUConfig) extends Plugin[MyCPUCore] {
  val tlbConfig = config.tlbConfig
  var CSRMan: CSRPlugin = null
  var excHandler: ExceptionHandlerPlugin = null

  // ! Registers
  // ! TLBIDX
  val TLBIDX_INDEX = RegInit(B(0x0, tlbConfig.indexWidth bits))
  val victim_idx = out(RegInit(U(0x0, tlbConfig.indexWidth bits)))
  val TLBIDX_PS = RegInit(B(0x0, 6 bits))
  val TLBIDX_NE = RegInit(False)

  // ! TLBEHI
  val TLBEHI_VPPN = RegInit(B(0x0, (31 downto 13).length bits))

  // ! TLBELO0, TLBELO1
  val TLBELO0_V = RegInit(False)
  val TLBELO0_D = RegInit(False)
  val TLBELO0_PLV = RegInit(B(0x0, 2 bits))
  val TLBELO0_MAT = RegInit(B(0x0, 2 bits))
  val TLBELO0_G = RegInit(False)
  val TLBELO0_PPN = RegInit(B(0x0, ((tlbConfig.physAddrWidth - 5) downto 8).length bits))
  val TLBELO1_V = RegInit(False)
  val TLBELO1_D = RegInit(False)
  val TLBELO1_PLV = RegInit(B(0x0, 2 bits))
  val TLBELO1_MAT = RegInit(B(0x0, 2 bits))
  val TLBELO1_G = RegInit(False)
  val TLBELO1_PPN = RegInit(B(0x0, ((tlbConfig.physAddrWidth - 5) downto 8).length bits))

  // ! ASID
  val ASID_ASID = RegInit(B(0x0, 10 bits))
  val ASID_ASIDBITS = B(0xa, (23 downto 16).length bits)

  // ! PGDL
  val PDGL_BASE = RegInit(B(0x0, 20 bits))

  // ! PGDH
  val PGDH_BASE = RegInit(B(0x0, 20 bits))

  // ! PGD
  val PGD = Bits(20 bits) // Mux(CSR.BADV(31), PGDH_BASE, PDGL_BASE)

  // ! TLBRENTRY
  val TLBRENTRY_PA = RegInit(B(0x0, 26 bits))

  // ! DMW0 ~ DMW1
  val DMW0_PLV0 = RegInit(B(0x0, 1 bits))
  val DMW0_PLV3 = RegInit(B(0x0, 1 bits))
  val DMW0_MAT = RegInit(B(0x0, 2 bits))
  val DMW0_PSEG = RegInit(B(0x0, 3 bits))
  val DMW0_VSEG = RegInit(B(0x0, 3 bits))
  val DMW1_PLV0 = RegInit(B(0x0, 1 bits))
  val DMW1_PLV3 = RegInit(B(0x0, 1 bits))
  val DMW1_MAT = RegInit(B(0x0, 2 bits))
  val DMW1_PSEG = RegInit(B(0x0, 3 bits))
  val DMW1_VSEG = RegInit(B(0x0, 3 bits))

  override def setup(pipeline: MyCPUCore): Unit = {
    import LoongArch.CSRAddress
    CSRMan = pipeline.service(classOf[CSRPlugin])
    excHandler = pipeline.service(classOf[ExceptionHandlerPlugin])

    // ! Registering CSR
    // ! TLBIDX (r0 is left out)
    CSRMan.rw(
      CSRAddress.TLBIDX,
      0 -> TLBIDX_INDEX,
      24 -> TLBIDX_PS,
      31 -> TLBIDX_NE
    )

    // ! TLBEHI
    CSRMan.rw(
      CSRAddress.TLBEHI,
      13 -> TLBEHI_VPPN
    )

    // ! TLBELO0
    CSRMan.rw(
      CSRAddress.TLBELO0,
      0 -> TLBELO0_V,
      1 -> TLBELO0_D,
      2 -> TLBELO0_PLV,
      4 -> TLBELO0_MAT,
      6 -> TLBELO0_G,
      8 -> TLBELO0_PPN
    )

    // ! TLBELO1
    CSRMan.rw(
      CSRAddress.TLBELO1,
      0 -> TLBELO1_V,
      1 -> TLBELO1_D,
      2 -> TLBELO1_PLV,
      4 -> TLBELO1_MAT,
      6 -> TLBELO1_G,
      8 -> TLBELO1_PPN
    )

    // ! ASID
    CSRMan.rw(
      CSRAddress.ASID,
      0 -> ASID_ASID
    )
    CSRMan.r(
      CSRAddress.ASID,
      16 -> ASID_ASIDBITS
    )

    // ! PGDL
    CSRMan.rw(CSRAddress.PGDL, 12 -> PDGL_BASE)

    // ! PGDH
    CSRMan.rw(CSRAddress.PGDH, 12 -> PGDH_BASE)

    // ! PGD
    PGD := Mux(excHandler.BADV_VADDR(31), PGDH_BASE, PDGL_BASE)
    CSRMan.r(CSRAddress.PGD, 12 -> PGD)

    // ! TLBRENTRY
    CSRMan.rw(CSRAddress.TLBRENTRY, 6 -> TLBRENTRY_PA)

    // ! DMW0
    CSRMan.rw(
      CSRAddress.DMW0,
      0 -> DMW0_PLV0,
      3 -> DMW0_PLV3,
      4 -> DMW0_MAT,
      25 -> DMW0_PSEG,
      29 -> DMW0_VSEG
    )

    // ! DMW1
    CSRMan.rw(
      CSRAddress.DMW1,
      0 -> DMW1_PLV0,
      3 -> DMW1_PLV3,
      4 -> DMW1_MAT,
      25 -> DMW1_PSEG,
      29 -> DMW1_VSEG
    )

  }

  // * Address Translate!

  // ! Direct Mapped
  def directTranslate(virtAddr: UInt, memOperation: NOP.constants.enum.MemOperationType.C) = new Area {
    val resultValid = Bool()
    val resultPhysAddr = UInt(32 bits)
    val resultCached = Bool()
    val resultExceptionBundle = new TranslateResultExceptionBundle

    // Parse DMW0
    val DMW0_valid: Bool =
      ((DMW0_PLV0.asBool && (excHandler.CRMD_PLV === 0)) || (DMW0_PLV3.asBool && excHandler.CRMD_PLV.asUInt === 3)) && (virtAddr(
        31 downto 29
      ).asBits === DMW0_VSEG)
    val DMW0_physAddr = DMW0_PSEG ## virtAddr(28 downto 0)
    val DMW0_cached = DMW0_MAT(0)

    // Parse DMW1
    val DMW1_valid: Bool =
      ((DMW1_PLV0.asBool && (excHandler.CRMD_PLV.asUInt === 0)) || (DMW1_PLV3.asBool && excHandler.CRMD_PLV.asUInt === 3)) && (virtAddr(
        31 downto 29
      ).asBits === DMW1_VSEG)
    val DMW1_physAddr = DMW1_PSEG ## virtAddr(28 downto 0)
    val DMW1_cached = DMW1_MAT(0)

    // Assemble
    resultValid := DMW0_valid || DMW1_valid
    resultPhysAddr := Mux(DMW0_valid, DMW0_physAddr, DMW1_physAddr).asUInt
    resultCached := Mux(DMW0_valid, DMW0_cached, DMW1_cached)

    // Return
    val resultBundle = Flow(TranslateResultBundle())
    resultBundle.valid := resultValid
    resultBundle.payload.physAddr := resultPhysAddr
    resultBundle.payload.cached := resultCached
    resultBundle.payload.exception := resultExceptionBundle
  }

  // ! Page Mapped
  final case class TLBEntry() extends Bundle {
    val E = RegInit(False)
    val ASID = RegInit(B(0x0, 10 bits))
    val G = RegInit(False)
    val PS = RegInit(U(0x0, 6 bits))
    val VPPN = RegInit(B(0x0, 19 bits))

    val V0 = RegInit(False)
    val D0 = RegInit(False)
    val MAT0 = RegInit(B(0x0, 2 bits))
    val PLV0 = RegInit(B(0x0, 2 bits))
    val PPN0 = RegInit(B(0x0, 20 bits))

    val V1 = RegInit(False)
    val D1 = RegInit(False)
    val MAT1 = RegInit(B(0x0, 2 bits))
    val PLV1 = RegInit(B(0x0, 2 bits))
    val PPN1 = RegInit(B(0x0, 20 bits))
  }
  val TLBTable = Vec(TLBEntry(), tlbConfig.numEntries)
  // TODO: [NOP] Remove debug signals
  val DebugTLBTableVPPN = out Vec (Bits(33 bits), tlbConfig.numEntries)
  for (i <- 0 until tlbConfig.numEntries) {
    DebugTLBTableVPPN(i) := TLBTable(i).E ## TLBTable(i).VPPN ## U(0, 13 bits)
  }
  val entryEnables = out Vec (TLBTable.map { entry => entry.E })
  val ASIDMatches = out Vec (TLBTable.map { entry => entry.ASID === ASID_ASID || entry.G })
  val VPPNMatches = out Vec (TLBTable.map { entry =>
    Mux(entry.PS === 12, entry.VPPN === TLBEHI_VPPN, entry.VPPN(18 downto 9) === TLBEHI_VPPN(18 downto 9))
  })
  // TODO: [NOP] Remove debug signals

  def tlbTranslate(virtAddr: UInt, memOperation: NOP.constants.enum.MemOperationType.C) = new Area {
    val resultValid = True
    val resultPhysAddr = UInt(32 bits)
    val resultCached = Bool()
    val resultExceptionBundle = new TranslateResultExceptionBundle

    val EntryEnabled = TLBTable.map { entry => entry.E }
    val ASIDMatches = TLBTable.map { entry => entry.G || entry.ASID === ASID_ASID }
    val VPPNMatches = Vec(TLBTable.map { entry =>
      Mux(
        entry.PS === 12,
        entry.VPPN === virtAddr(31 downto 13).asBits,
        entry.VPPN(18 downto 9) === virtAddr(31 downto 22).asBits
      )
    })

    val EntryHits = Seq.fill(tlbConfig.numEntries)(Bool())
    for (i <- 0 until tlbConfig.numEntries) {
      EntryHits(i) := EntryEnabled(i) && ASIDMatches(i) && VPPNMatches(i)
    }

    val TLBHit = EntryHits.orR
    val TLBHitEntry = MuxOH(Vec(EntryHits), TLBTable)

    // Extract entry. Use (virtAddr[TLBHitEntry.PS]) as mux.
    val TLBHitEntry_v = Mux(virtAddr(TLBHitEntry.PS(4 downto 0)), TLBHitEntry.V1, TLBHitEntry.V0)
    val TLBHitEntry_d = Mux(virtAddr(TLBHitEntry.PS(4 downto 0)), TLBHitEntry.D1, TLBHitEntry.D0)
    val TLBHitEntry_mat = Mux(virtAddr(TLBHitEntry.PS(4 downto 0)), TLBHitEntry.MAT1, TLBHitEntry.MAT0)
    val TLBHitEntry_plv = Mux(virtAddr(TLBHitEntry.PS(4 downto 0)), TLBHitEntry.PLV1, TLBHitEntry.PLV0)
    val TLBHitEntry_ppn = Mux(virtAddr(TLBHitEntry.PS(4 downto 0)), TLBHitEntry.PPN1, TLBHitEntry.PPN0)

    when(~TLBHit) {
      resultExceptionBundle.raiseTLBR.set()
    }

    when(~TLBHitEntry_v) {
      switch(memOperation) {
        is(MemOperationType.FETCH) {
          resultExceptionBundle.raisePIF := True
        }
        is(MemOperationType.LOAD) {
          resultExceptionBundle.raisePIL := True
        }
        is(MemOperationType.STORE) {
          resultExceptionBundle.raisePIS := True
        }
      }
    }

    when(excHandler.CRMD_PLV.asUInt > TLBHitEntry_plv.asUInt) {
      resultExceptionBundle.raisePPI.set()
    }

    when(memOperation === MemOperationType.STORE && ~TLBHitEntry_d) {
      resultExceptionBundle.raisePME.set()
    }

    // (31 downto PS) is 1, (PS - 1 downto 0) is 0
    resultPhysAddr := Mux(
      TLBHitEntry.PS === 12,
      TLBHitEntry_ppn ## virtAddr(11 downto 0),
      TLBHitEntry_ppn(19 downto 9) ## virtAddr(20 downto 0)
    ).asUInt
    resultCached := TLBHitEntry_mat(0)

    val resultBundle = Flow(TranslateResultBundle())
    resultBundle.valid := resultValid
    resultBundle.payload.physAddr := resultPhysAddr
    resultBundle.payload.cached := resultCached
    resultBundle.payload.exception := resultExceptionBundle
  }

  def translate(
      virtAddr: UInt,
      memOperation: NOP.constants.enum.MemOperationType.C,
      directTranslateResult: Flow[TranslateResultBundle],
      tlbTranslateResult: Flow[TranslateResultBundle],
      savedCSR: TranslateCSRBundle
  ) = new Area {
    val resultValid = False
    val resultPhysAddr = U(0x0, 32 bits)
    val resultCached = False
    val resultExceptionBundle = new TranslateResultExceptionBundle

    // 映射地址翻译
    when(~savedCSR.CRMD_DA && savedCSR.CRMD_PG && tlbTranslateResult.valid) {
      // ! 页表映射模式
      resultValid := tlbTranslateResult.valid
      resultPhysAddr := tlbTranslateResult.payload.physAddr
      resultCached := tlbTranslateResult.payload.cached
      resultExceptionBundle := tlbTranslateResult.payload.exception
    }

    when(~savedCSR.CRMD_DA && savedCSR.CRMD_PG && directTranslateResult.valid) {
      // ! 直接映射模式
      resultValid := directTranslateResult.valid
      resultPhysAddr := directTranslateResult.payload.physAddr
      resultCached := directTranslateResult.payload.cached
      resultExceptionBundle := directTranslateResult.payload.exception
    }

    when(savedCSR.CRMD_DA && ~savedCSR.CRMD_PG) {
      // 直接地址翻译
      resultValid := True
      resultPhysAddr := virtAddr
      when(memOperation === MemOperationType.FETCH) {
        resultCached := savedCSR.CRMD_DATF(0)
      } otherwise {
        resultCached := savedCSR.CRMD_DATM(0)
      }
      resultExceptionBundle := new TranslateResultExceptionBundle
    }

    // Return
    val resultBundle = Flow(TranslateResultBundle())
    resultBundle.valid := resultValid
    resultBundle.payload.physAddr := resultPhysAddr
    resultBundle.payload.cached := resultCached
    resultBundle.payload.exception := resultExceptionBundle
  }

  // ! Resolve TLB Instructions
  def ParseTLBInstruction(pipeline: MyCPUCore) = new Area {
    val CommitPlugin = pipeline.service(classOf[CommitPlugin])
    val tlbOp = CommitPlugin.tlbOp
    val tlbInvASID = CommitPlugin.tlbInvASID
    val tlbInvVPPN = CommitPlugin.tlbInvVPPN

    switch(tlbOp) {
      is(TLBOpType.NONE) {
        // Great! Do nothing.
      }
      is(TLBOpType.TLBSRCH) {
        val entryEnables = Vec(TLBTable.map { entry => entry.E })
        val ASIDMatches = Vec(TLBTable.map { entry => entry.ASID === ASID_ASID || entry.G })
        val VPPNMatches = Vec(TLBTable.map { entry =>
          Mux(entry.PS === 12, entry.VPPN === TLBEHI_VPPN, entry.VPPN(18 downto 9) === TLBEHI_VPPN(18 downto 9))
        })

        val EntryHits = Vec(Bool(), config.tlbConfig.numEntries)
        for (i <- (0 until tlbConfig.numEntries)) {
          EntryHits(i) := entryEnables(i) && ASIDMatches(i) && VPPNMatches(i)
        }

        val Indices = Vec(Bits(tlbConfig.indexWidth bits), config.tlbConfig.numEntries)
        for (i <- (0 until tlbConfig.numEntries)) {
          Indices(i) := B(i, tlbConfig.indexWidth bits)
        }

        val EntryHit = EntryHits.orR
        when(EntryHit) {
          TLBIDX_INDEX := MuxOH(EntryHits, Indices)
          TLBIDX_NE := False
        } otherwise {
          TLBIDX_NE := True
        }
      }
      is(TLBOpType.TLBRD) {
        val TLBEntry = TLBTable(TLBIDX_INDEX.asUInt)
        when(TLBEntry.E) {
          TLBEHI_VPPN := TLBEntry.VPPN
          TLBIDX_PS := TLBEntry.PS.asBits
          TLBIDX_NE := False
          ASID_ASID := TLBEntry.ASID

          TLBELO0_D := TLBEntry.D0
          TLBELO0_V := TLBEntry.V0
          TLBELO0_G := TLBEntry.G
          TLBELO0_PLV := TLBEntry.PLV0
          TLBELO0_PPN := TLBEntry.PPN0
          TLBELO0_MAT := TLBEntry.MAT0

          TLBELO1_D := TLBEntry.D1
          TLBELO1_V := TLBEntry.V1
          TLBELO1_G := TLBEntry.G
          TLBELO1_PLV := TLBEntry.PLV1
          TLBELO1_PPN := TLBEntry.PPN1
          TLBELO1_MAT := TLBEntry.MAT1
        } otherwise {
          TLBIDX_NE := True
          ASID_ASID := 0
          TLBEHI_VPPN := 0
          TLBIDX_PS := 0

          TLBELO0_MAT := 0
          TLBELO0_G := False
          TLBELO0_PLV := 0
          TLBELO0_PPN := 0
          TLBELO0_V := False
          TLBELO0_D := False

          TLBELO1_MAT := 0
          TLBELO1_G := False
          TLBELO1_PLV := 0
          TLBELO1_PPN := 0
          TLBELO1_V := False
          TLBELO1_D := False
        }
      }
      is(TLBOpType.TLBWR) {
        val TLBEntry = TLBTable(TLBIDX_INDEX.asUInt)
        when(excHandler.ESTAT_ECODE === 0x3f) {
          TLBEntry.E := True
        } otherwise {
          TLBEntry.E := !TLBIDX_NE
        }
        TLBEntry.ASID := ASID_ASID
        TLBEntry.VPPN := TLBEHI_VPPN
        TLBEntry.D0 := TLBELO0_D
        TLBEntry.V0 := TLBELO0_V
        TLBEntry.G := TLBELO0_G && TLBELO1_G
        TLBEntry.PLV0 := TLBELO0_PLV
        TLBEntry.PPN0 := TLBELO0_PPN
        TLBEntry.MAT0 := TLBELO0_MAT
        TLBEntry.D1 := TLBELO1_D
        TLBEntry.V1 := TLBELO1_V
        TLBEntry.PLV1 := TLBELO1_PLV
        TLBEntry.PPN1 := TLBELO1_PPN
        TLBEntry.MAT1 := TLBELO1_MAT
        TLBEntry.PS := TLBIDX_PS.asUInt
      }
      is(TLBOpType.TLBFILL) {
        // First select a victim entry
        victim_idx := victim_idx + 1
        val TLBEntry = TLBTable(victim_idx)
        when(excHandler.ESTAT_ECODE === 0x3f) {
          TLBEntry.E := True
        } otherwise {
          TLBEntry.E := !TLBIDX_NE
        }
        TLBEntry.VPPN := TLBEHI_VPPN
        TLBEntry.PS := TLBIDX_PS.asUInt
        TLBEntry.G := TLBELO0_G && TLBELO1_G
        TLBEntry.ASID := ASID_ASID

        TLBEntry.D0 := TLBELO0_D
        TLBEntry.V0 := TLBELO0_V
        TLBEntry.PLV0 := TLBELO0_PLV
        TLBEntry.PPN0 := TLBELO0_PPN
        TLBEntry.MAT0 := TLBELO0_MAT

        TLBEntry.D1 := TLBELO1_D
        TLBEntry.V1 := TLBELO1_V
        TLBEntry.PLV1 := TLBELO1_PLV
        TLBEntry.PPN1 := TLBELO1_PPN
        TLBEntry.MAT1 := TLBELO1_MAT

      }
      default {
        // INVTLB
        val allMask = Vec(True, tlbConfig.numEntries)
        val G1Mask = TLBTable.map { entry => entry.G }
        val ASIDMatchMask = TLBTable.map { entry => entry.ASID === tlbInvASID }
        val VPPNMatchMask = TLBTable.map { entry =>
          Mux(entry.PS === 12, entry.VPPN === tlbInvVPPN, entry.VPPN(18 downto 10) === tlbInvVPPN(18 downto 10))
        }
        val InvMask = Vec(Bool(), tlbConfig.numEntries)

        switch(tlbOp) {
          is(TLBOpType.INVTLB2) {
            for (i <- (0 until tlbConfig.numEntries)) {
              InvMask(i) := G1Mask(i)
            }
          }
          is(TLBOpType.INVTLB3) {
            for (i <- (0 until tlbConfig.numEntries)) {
              InvMask(i) := !G1Mask(i)
            }
          }
          is(TLBOpType.INVTLB4) {
            for (i <- (0 until tlbConfig.numEntries)) {
              InvMask(i) := !G1Mask(i) && ASIDMatchMask(i)
            }
          }
          is(TLBOpType.INVTLB5) {
            for (i <- (0 until tlbConfig.numEntries)) {
              InvMask(i) := !G1Mask(i) && ASIDMatchMask(i) && VPPNMatchMask(i)
            }
          }
          is(TLBOpType.INVTLB6) {
            for (i <- (0 until tlbConfig.numEntries)) {
              InvMask(i) := (G1Mask(i) || ASIDMatchMask(i)) && VPPNMatchMask(i)
            }
          }
          default {
            for (i <- (0 until tlbConfig.numEntries)) {
              InvMask(i) := allMask(i)
            }
          }
        }

        // Invalid InvMask!
        for (i <- (0 until tlbConfig.numEntries)) {
          when(InvMask(i)) {
            val TLBEntry = TLBTable(i)
            TLBEntry.E := False
          }
        }
      }
    }

  }

  override def build(pipeline: MyCPUCore): Unit = {
    ParseTLBInstruction(pipeline)
  }

}
