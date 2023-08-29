package NOP

import NOP.constants.LoongArch.CSRAddress
import NOP.constants.`enum`.{CacheOpType, CacheSelType, LoadStoreType, TLBOpType}
import spinal.core.{Bits, _}
import spinal.lib.bus.amba4.axi._
import spinal.lib._
import NOP.debug._
import NOP.pipeline.core._
import NOP.pipeline.exe.MemIssueQueuePlugin
import NOP.pipeline.priviledge._
import NOP.utils._

class MyCPU(config: MyCPUConfig) extends Component {
  setDefinitionName("mycpu_top")
  noIoPrefix()

  val io = new Bundle {
    val aclk = in(Bool)
    val aresetn = in(Bool)
    // Interrupts
    val intrpt = in(Bits(8 bits))

    // AXI Interface
    val axi = master(Axi4(config.axiConfig)).setName("")
    val wid = out(UInt(4 bits))

    // Debug Interface
    val debug0 = out(new DebugInterface())

    // For chiplab
    val break_point = in(Bool())
    val infor_flag = in(Bool())
    val reg_num = in(UInt(5 bits))

    val ws_valid = out(Bool())
    val rf_rdata = out(Bits(32 bits))

    // For our own debug usage
    val DretireMask = out(Bits(config.rob.retireWidth bits))
    val DretireAddr = out(Vec(Bits(32 bits), config.rob.retireWidth))
    val DretireInst = out(Vec(Bits(32 bits), config.rob.retireWidth))
    val DretireWen = out(Bits(config.rob.retireWidth bits))
    val DretireWaddr = out(Vec(Bits(5 bits), config.rob.retireWidth))
    val DretireWresult = out(Vec(Bits(32 bits), config.rob.retireWidth))
    val DuniqueRetire = out(Bits(config.rob.retireWidth bits))
    val DINT0EXE = out(BWord())
    val DINT1EXE = out(BWord())
    val DINT2EXE = out(BWord())
    val DaRAT_val = out(Vec(Bits(32 bits), config.regFile.nArchRegs - 1))
    val DphysRegFile = out(Vec(Bits(32 bits), config.regFile.nPhysRegs))
    val DsRAT = out(Vec(Bits(config.regFile.prfAddrWidth bits), config.regFile.nArchRegs - 1))
    val Dexcept = out(Bool())
    val DexceptCode = out(Bits(6 bits))
    val DcsrValid = out(Bool())
    val DcsrAddr = out(Bits(14 bits))
    val DcsrData = out(Bits(32 bits))
    val DECFG = out(Bits(12 bits))
    val DESTAT = out(Bits(12 bits))
    val DTIMEVAL = out(Bits(32 bits))
    val DTLBTableVPPN = out Vec (Bits(33 bits), config.tlbConfig.numEntries)
    val DEntryMatches = out Bits (config.tlbConfig.numEntries bits)
    val DASIDMatches = out Bits (config.tlbConfig.numEntries bits)
    val DVPPNMatches = out Bits (config.tlbConfig.numEntries bits)
    val DCacheValid = out Bool ()
    val DCacheOpCode = out Bits (CacheOpType.None.asBits.getWidth bits)
    val DCacheSelCode = out Bits (CacheSelType.None.asBits.getWidth bits)
    val DuncachedMask = out Bits (3 bits)
    val DMEMIssueQueueValid = out Bits (config.memIssue.depth bits)
    val DMEMIssueQueuePc = out Vec (Bits(32 bits), config.memIssue.depth)

    val DifftestBundle = out(new Bundle {
      // For Difftest Debug Usage
      /*
      input [ 7:0] index,
      input        valid,
      input [63:0] pc,
      input [31:0] instr,
      input        skip,
      input        is_TLBFILL,
      input [ 4:0] TLBFILL_index,
      input        is_CNTinst,
      input [63:0] timer_64_value,
      input        wen,
      input [ 7:0] wdest,
      input [63:0] wdata,
      input        csr_rstat,
      input [31:0] csr_data
       */
      val DifftestInstrCommitIndex = out Vec (out Bits (8 bits), config.rob.retireWidth)
      val DifftestInstrCommitValid = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestInstrCommitPC = out Vec (out Bits (64 bits), config.rob.retireWidth)
      val DifftestInstrCommitInstr = out Vec (out Bits (32 bits), config.rob.retireWidth)
      val DifftestSkip = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestIsTlbFill = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestTlbFillIndex = out Vec (out Bits (5 bits), config.rob.retireWidth)
      val DifftestIsCount = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestCount = out Vec (out Bits (64 bits), config.rob.retireWidth)
      val DifftestWen = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestWdest = out Vec (out Bits (8 bits), config.rob.retireWidth)
      val DifftestWdata = out Vec (out Bits (64 bits), config.rob.retireWidth)
      val DifftestCsrRstat = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestCsrData = out Vec (out Bits (32 bits), config.rob.retireWidth)

      /*
      input        excp_valid,
      input        eret,
      input [31:0] intrNo,
      input [31:0] cause,
      input [63:0] exceptionPC,
      input [31:0] exceptionInst
       * */
      val DifftestExcpEventExcpValid = out Bool ()
      val DifftestExcpEventEret = out Bool ()
      val DifftestExcpEventIntrNO = out Bits (32 bits)
      val DifftestExcpEventCause = out Bits (32 bits)
      val DifftestExcpEventEPC = out Bits (64 bits)
      val DifftestExcpEventInst = out Bits (32 bits)

      /*
       * DifftestTrapEvent
       * */
      val DifftestTrapEventValid = out(False)

      /*
       * DifftestStoreEvent
      input [ 7:0] valid,
      input [63:0] storePAddr,
      input [63:0] storeVAddr,
      input [63:0] storeData
       * */
      val DifftestStoreEventValid = out(Bits(8 bits))
      val DifftestStoreEventPAddr = out(Bits(64 bits))
      val DifftestStoreEventVAddr = out(Bits(64 bits))
      val DifftestStoreEventData = out(Bits(64 bits))

      /*
      DifftestLoadEvent
      input [ 7:0] valid,
      input [63:0] paddr,
      input [63:0] vaddr
       */
      val DifftestLoadEventValid = out(Bits(8 bits))
      val DifftestLoadEventPAddr = out(Bits(64 bits))
      val DifftestLoadEventVAddr = out(Bits(64 bits))

      /*
       * DifftestCSRRegState
      input [63:0] crmd,
      input [63:0] prmd,
      input [63:0] euen,
      input [63:0] ecfg,
      input [63:0] estat,
      input [63:0] era,
      input [63:0] badv,
      input [63:0] eentry,
      input [63:0] tlbidx,
      input [63:0] tlbehi,
      input [63:0] tlbelo0,
      input [63:0] tlbelo1,
      input [63:0] asid,
      input [63:0] pgdl,
      input [63:0] pgdh,
      input [63:0] save0,
      input [63:0] save1,
      input [63:0] save2,
      input [63:0] save3,
      input [63:0] tid,
      input [63:0] tcfg,
      input [63:0] tval,
      input [63:0] ticlr,
      input [63:0] llbctl,
      input [63:0] tlbrentry,
      input [63:0] dmw0,
      input [63:0] dmw1
       */
      val DifftestCSRRegStateCRMD = out(Bits(64 bits))
      val DifftestCSRRegStatePRMD = out(Bits(64 bits))
      val DifftestCSRRegStateEUEN = out(Bits(64 bits))
      val DifftestCSRRegStateECFG = out(Bits(64 bits))
      val DifftestCSRRegStateESTAT = out(Bits(64 bits))
      val DifftestCSRRegStateERA = out(Bits(64 bits))
      val DifftestCSRRegStateBADV = out(Bits(64 bits))
      val DifftestCSRRegStateEENTRY = out(Bits(64 bits))
      val DifftestCSRRegStateTLBIDX = out(Bits(64 bits))
      val DifftestCSRRegStateTLBEHI = out(Bits(64 bits))
      val DifftestCSRRegStateTLBELO0 = out(Bits(64 bits))
      val DifftestCSRRegStateTLBELO1 = out(Bits(64 bits))
      val DifftestCSRRegStateASID = out(Bits(64 bits))
      val DifftestCSRRegStatePGDL = out(Bits(64 bits))
      val DifftestCSRRegStatePGDH = out(Bits(64 bits))
      val DifftestCSRRegStateSAVE0 = out(Bits(64 bits))
      val DifftestCSRRegStateSAVE1 = out(Bits(64 bits))
      val DifftestCSRRegStateSAVE2 = out(Bits(64 bits))
      val DifftestCSRRegStateSAVE3 = out(Bits(64 bits))
      val DifftestCSRRegStateTID = out(Bits(64 bits))
      val DifftestCSRRegStateTCFG = out(Bits(64 bits))
      val DifftestCSRRegStateTVAL = out(Bits(64 bits))
      val DifftestCSRRegStateTICLR = out(Bits(64 bits))
      val DifftestCSRRegStateLLBCTL = out(Bits(64 bits))
      val DifftestCSRRegStateTLBRENTRY = out(Bits(64 bits))
      val DifftestCSRRegStateDMW0 = out(Bits(64 bits))
      val DifftestCSRRegStateDMW1 = out(Bits(64 bits))

      /*
      DifftestGRegState
       */
      // PLZ refer to aRAT
    })

    val DifftestDelayBundle = out((new Bundle {
      // For Difftest Debug Usage
      /*
      input [ 7:0] index,
      input        valid,
      input [63:0] pc,
      input [31:0] instr,
      input        skip,
      input        is_TLBFILL,
      input [ 4:0] TLBFILL_index,
      input        is_CNTinst,
      input [63:0] timer_64_value,
      input        wen,
      input [ 7:0] wdest,
      input [63:0] wdata,
      input        csr_rstat,
      input [31:0] csr_data
       */
      val DifftestInstrCommitIndex = out Vec (out Bits (8 bits), config.rob.retireWidth)
      val DifftestInstrCommitValid = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestInstrCommitPC = out Vec (out Bits (64 bits), config.rob.retireWidth)
      val DifftestInstrCommitInstr = out Vec (out Bits (32 bits), config.rob.retireWidth)
      val DifftestSkip = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestIsTlbFill = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestTlbFillIndex = out Vec (out Bits (5 bits), config.rob.retireWidth)
      val DifftestIsCount = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestCount = out Vec (out Bits (64 bits), config.rob.retireWidth)
      val DifftestWen = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestWdest = out Vec (out Bits (8 bits), config.rob.retireWidth)
      val DifftestWdata = out Vec (out Bits (64 bits), config.rob.retireWidth)
      val DifftestCsrRstat = out Vec (out Bool (), config.rob.retireWidth)
      val DifftestCsrData = out Vec (out Bits (32 bits), config.rob.retireWidth)

      /*
      input        excp_valid,
      input        eret,
      input [31:0] intrNo,
      input [31:0] cause,
      input [63:0] exceptionPC,
      input [31:0] exceptionInst
       * */
      val DifftestExcpEventExcpValid = out Bool ()
      val DifftestExcpEventEret = out Bool ()
      val DifftestExcpEventIntrNO = out Bits (32 bits)
      val DifftestExcpEventCause = out Bits (32 bits)
      val DifftestExcpEventEPC = out Bits (64 bits)
      val DifftestExcpEventInst = out Bits (32 bits)

      /*
       * DifftestStoreEvent
      input [ 7:0] valid,
      input [63:0] storePAddr,
      input [63:0] storeVAddr,
      input [63:0] storeData
       * */
      val DifftestStoreEventValid = out(Bits(8 bits))
      val DifftestStoreEventPAddr = out(Bits(64 bits))
      val DifftestStoreEventVAddr = out(Bits(64 bits))
      val DifftestStoreEventData = out(Bits(64 bits))

      /*
      DifftestLoadEvent
      input [ 7:0] valid,
      input [63:0] paddr,
      input [63:0] vaddr
       */
      val DifftestLoadEventValid = out(Bits(8 bits))
      val DifftestLoadEventPAddr = out(Bits(64 bits))
      val DifftestLoadEventVAddr = out(Bits(64 bits))
    }))

  }

  val defaultClockDomain = ClockDomain(
    clock = io.aclk,
    reset = io.aresetn,
    config = ClockDomainConfig(resetActiveLevel = LOW)
  )

  val defaultClockArea = new ClockingArea(defaultClockDomain) {
    val cpu = new NOP.pipeline.core.MyCPUCore(config)

    // Connect AXI
    io.wid := RegNextWhen(io.axi.aw.id, io.axi.aw.valid, U(0))

    // Connect io to CPUCore
    cpu.io.intrpt := io.intrpt
    if (config.debug) {
      io.debug0 := cpu.io.debug
    } else {
      io.debug0.assignDontCare()
    }

    // arbitor
    val crossbar = new NOP.peripheral.AxiCrossbar(config.axiConfig)
    cpu.io.iBus >> crossbar.io.iBus
    cpu.io.dBus >> crossbar.io.dBus

    crossbar.io.cpuBus >> io.axi

    // udBus: uncached DBus
    val axiBuffer = new NOP.peripheral.AxiBuffer(config.axiConfig)
    cpu.io.udBus <> axiBuffer.io.in_axi
    axiBuffer.io.out_axi >> crossbar.io.udBus

    // Debug Area
    config.weDebug generate {
      val ROBFIFOPlugin = cpu.service(classOf[ROBFIFOPlugin])
      io.DretireAddr := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
        stream_entry_bundle.info.uop.pc.asBits
      }))
      io.DretireMask := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map { stream_entry_bundle =>
        stream_entry_bundle.valid && stream_entry_bundle.ready
      }).asBits
      io.DretireInst := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
        stream_entry_bundle.info.uop.inst.asBits
      }))
      io.DretireWen := ROBFIFOPlugin.debug_fifoIO.pop
        .map({ stream_entry_bundle => stream_entry_bundle.info.uop.doRegWrite.asBits })
        .asBits()
      io.DretireWaddr := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
        stream_entry_bundle.info.uop.wbAddr.asBits
      }))
      io.DuniqueRetire := ROBFIFOPlugin.debug_fifoIO.pop
        .map({ stream_entry_bundle => stream_entry_bundle.info.uop.uniqueRetire.asBits })
        .asBits()
      io.DretireWresult := Vec(ROBFIFOPlugin.debugPopWriteData)

      val PhysRegFilePlugin = cpu.service(classOf[PhysRegFilePlugin])
      io.DphysRegFile := PhysRegFilePlugin.debug_regs

      io.DINT0EXE := cpu.exePipelines(0).get_signal("exeResult").asBits
      io.DINT1EXE := cpu.exePipelines(1).get_signal("exeResult").asBits
      io.DINT2EXE := cpu.exePipelines(2).get_signal("exeResult").asBits

      val debug_sRAT = cpu.decodePipeline.get_signal("sRAT")
      io.DsRAT.assignFromBits(debug_sRAT)

      val debug_aRAT = cpu.decodePipeline.get_signal("aRAT")
      val aRAT = Vec(Bits(config.regFile.prfAddrWidth bits), config.regFile.nArchRegs - 1)
      aRAT.assignFromBits(debug_aRAT)
      io.DaRAT_val := Vec(aRAT.map { prf_addr =>
        PhysRegFilePlugin.debug_regs(prf_addr.asUInt - U(1, config.regFile.prfAddrWidth bits))
      })

      val CommitPlugin = cpu.service(classOf[CommitPlugin])
      io.Dexcept := CommitPlugin.except.valid
      io.DexceptCode := CommitPlugin.except.payload.code

      io.DcsrValid := CommitPlugin.CSRWrite.valid
      io.DcsrAddr := CommitPlugin.CSRWrite.payload.addr.asBits
      io.DcsrData := CommitPlugin.CSRWrite.payload.data.asBits

      val InterruptPlugin = cpu.service(classOf[InterruptHandlerPlugin])
      io.DECFG := InterruptPlugin.ECFG
      io.DESTAT := InterruptPlugin.ESTAT
      io.DTIMEVAL := InterruptPlugin.TVAL_TIMEVAL.asBits

      val MMUPlugin = cpu.service(classOf[MMUPlugin])
      io.DTLBTableVPPN := MMUPlugin.DebugTLBTableVPPN
      io.DEntryMatches := MMUPlugin.entryEnables.asBits
      io.DASIDMatches := MMUPlugin.ASIDMatches.asBits
      io.DVPPNMatches := MMUPlugin.VPPNMatches.asBits

      io.DCacheValid := CommitPlugin.cacheOp.valid
      io.DCacheOpCode := CommitPlugin.cacheOp.payload.op.asBits
      io.DCacheSelCode := CommitPlugin.cacheOp.payload.sel.asBits

      io.DuncachedMask := CommitPlugin.DuncachedMask

      val memIssueQueue = cpu.service(classOf[MemIssueQueuePlugin])
      io.DMEMIssueQueueValid := memIssueQueue.queue.map { entry => entry.valid }.asBits
      io.DMEMIssueQueuePc := Vec(memIssueQueue.queue.map({ entry => entry.uop.pc.asBits }))

      if (config.debug_difftest) {
        // InstrCommit
        io.DifftestBundle.DifftestInstrCommitIndex := Vec(Seq(B(0x0, 8 bits), B(0x1, 8 bits), B(0x2, 8 bits)))
        io.DifftestBundle.DifftestInstrCommitValid := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map { stream_entry_bundle =>
          stream_entry_bundle.valid && stream_entry_bundle.ready
        })
        io.DifftestBundle.DifftestInstrCommitPC := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.info.uop.pc.asBits.resized
        }))
        io.DifftestBundle.DifftestInstrCommitInstr := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.info.uop.inst.asBits
        }))
        io.DifftestBundle.DifftestSkip := Vec(Seq(False, False, False))
        io.DifftestBundle
          .DifftestIsTlbFill(0) := ROBFIFOPlugin.debug_fifoIO.pop(0).payload.info.uop.tlbOp === TLBOpType.TLBFILL
        io.DifftestBundle.DifftestTlbFillIndex(0) := MMUPlugin.victim_idx.asBits.resized
        for (i <- 1 until config.rob.retireWidth) {
          io.DifftestBundle.DifftestIsTlbFill(i) := False
          io.DifftestBundle.DifftestTlbFillIndex(i) := 0
        }

        io.DifftestBundle.DifftestIsCount := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.info.uop.readTimer64L || stream_entry_bundle.info.uop.readTimer64H || stream_entry_bundle.info.uop.readTimer64ID
        }))
        io.DifftestBundle.DifftestCount := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.state.count64ReadValue.asBits
        }))
        io.DifftestBundle.DifftestWen := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.info.uop.doRegWrite
        }))
        io.DifftestBundle.DifftestWdest := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.info.uop.wbAddr.asBits.resized
        }))
        io.DifftestBundle.DifftestWdata := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          PhysRegFilePlugin.debug_regs(stream_entry_bundle.info.rename.wReg).asBits.resized
        }))
        io.DifftestBundle.DifftestCsrRstat := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          (stream_entry_bundle.info.uop.readCSR || stream_entry_bundle.info.uop.writeCSR) && stream_entry_bundle.info.uop
            .inst(23 downto 10) === CSRAddress.ESTAT
        }))
        io.DifftestBundle.DifftestCsrData := Vec(ROBFIFOPlugin.debug_fifoIO.pop.map({ stream_entry_bundle =>
          stream_entry_bundle.state.csrRdata.asBits
        }))

        // ExcpEvent
        val excHandler = cpu.service(classOf[ExceptionHandlerPlugin])
        io.DifftestBundle.DifftestExcpEventExcpValid := CommitPlugin.except.valid
        io.DifftestBundle.DifftestExcpEventEret := CommitPlugin.ertn
        io.DifftestBundle.DifftestExcpEventIntrNO := (excHandler.ESTAT_IS_12 ## excHandler.ESTAT_IS_11 ## B(
          0,
          1 bits
        ) ## excHandler.ESTAT_IS_2).resized
        io.DifftestBundle.DifftestExcpEventCause := CommitPlugin.except.payload.code.resized
        io.DifftestBundle.DifftestExcpEventEPC := ROBFIFOPlugin.debug_fifoIO.pop(0).info.uop.pc.asBits.resized
        io.DifftestBundle.DifftestExcpEventInst := ROBFIFOPlugin.debug_fifoIO.pop(0).info.uop.inst

        val currInstr = ROBFIFOPlugin.debug_fifoIO.pop(0)
        io.DifftestBundle.DifftestStoreEventValid := Mux(
          currInstr.fire && currInstr.info.uop.isStore && !CommitPlugin.except.valid,
          B(
            0,
            4 bits
          ) ## (currInstr.info.uop.isSC && excHandler.LLBCTL_LLBIT) ## (!currInstr.info.uop.isSC && currInstr.info.uop.lsType === LoadStoreType.WORD) ## (currInstr.info.uop.lsType === LoadStoreType.HALF) ## (currInstr.info.uop.lsType === LoadStoreType.BYTE),
          B"00000000"
        )
        io.DifftestBundle.DifftestStoreEventVAddr := currInstr.state.vAddr.asBits.resized
        io.DifftestBundle.DifftestStoreEventPAddr := currInstr.state.pAddr.asBits.resized
        io.DifftestBundle.DifftestStoreEventData := currInstr.state.storeData.asBits.resized

        io.DifftestBundle.DifftestLoadEventValid := Mux(
          currInstr.fire && currInstr.info.uop.isLoad && !CommitPlugin.except.valid,
          B(
            0,
            2 bits
          ) ## currInstr.info.uop.isLL ## (currInstr.info.uop.lsType === LoadStoreType.WORD) ## (currInstr.info.uop.lsType === LoadStoreType.HALF_U) ## (currInstr.info.uop.lsType === LoadStoreType.HALF) ## (currInstr.info.uop.lsType === LoadStoreType.BYTE_U) ## (currInstr.info.uop.lsType === LoadStoreType.BYTE),
          B"00000000"
        )
        io.DifftestBundle.DifftestLoadEventVAddr := currInstr.state.vAddr.asBits.resized
        io.DifftestBundle.DifftestLoadEventPAddr := currInstr.state.pAddr.asBits.resized

        val CSRMan = cpu.service(classOf[CSRPlugin])
        io.DifftestBundle.DifftestCSRRegStateCRMD := CSRMan.DifftestCSRRegStateCRMD
        io.DifftestBundle.DifftestCSRRegStatePRMD := CSRMan.DifftestCSRRegStatePRMD
        io.DifftestBundle.DifftestCSRRegStateEUEN := CSRMan.DifftestCSRRegStateEUEN
        io.DifftestBundle.DifftestCSRRegStateECFG := CSRMan.DifftestCSRRegStateECFG
        io.DifftestBundle.DifftestCSRRegStateESTAT := CSRMan.DifftestCSRRegStateESTAT
        io.DifftestBundle.DifftestCSRRegStateERA := CSRMan.DifftestCSRRegStateERA
        io.DifftestBundle.DifftestCSRRegStateBADV := CSRMan.DifftestCSRRegStateBADV
        io.DifftestBundle.DifftestCSRRegStateEENTRY := CSRMan.DifftestCSRRegStateEENTRY
        io.DifftestBundle.DifftestCSRRegStateTLBIDX := CSRMan.DifftestCSRRegStateTLBIDX
        io.DifftestBundle.DifftestCSRRegStateTLBEHI := CSRMan.DifftestCSRRegStateTLBEHI
        io.DifftestBundle.DifftestCSRRegStateTLBELO0 := CSRMan.DifftestCSRRegStateTLBELO0
        io.DifftestBundle.DifftestCSRRegStateTLBELO1 := CSRMan.DifftestCSRRegStateTLBELO1
        io.DifftestBundle.DifftestCSRRegStateASID := CSRMan.DifftestCSRRegStateASID
        io.DifftestBundle.DifftestCSRRegStatePGDL := CSRMan.DifftestCSRRegStatePGDL
        io.DifftestBundle.DifftestCSRRegStatePGDH := CSRMan.DifftestCSRRegStatePGDH
        io.DifftestBundle.DifftestCSRRegStateSAVE0 := CSRMan.DifftestCSRRegStateSAVE0
        io.DifftestBundle.DifftestCSRRegStateSAVE1 := CSRMan.DifftestCSRRegStateSAVE1
        io.DifftestBundle.DifftestCSRRegStateSAVE2 := CSRMan.DifftestCSRRegStateSAVE2
        io.DifftestBundle.DifftestCSRRegStateSAVE3 := CSRMan.DifftestCSRRegStateSAVE3
        io.DifftestBundle.DifftestCSRRegStateTID := CSRMan.DifftestCSRRegStateTID
        io.DifftestBundle.DifftestCSRRegStateTCFG := CSRMan.DifftestCSRRegStateTCFG
        io.DifftestBundle.DifftestCSRRegStateTVAL := CSRMan.DifftestCSRRegStateTVAL
        io.DifftestBundle.DifftestCSRRegStateTICLR := CSRMan.DifftestCSRRegStateTICLR
        io.DifftestBundle.DifftestCSRRegStateLLBCTL := CSRMan.DifftestCSRRegStateLLBCTL
        io.DifftestBundle.DifftestCSRRegStateTLBRENTRY := CSRMan.DifftestCSRRegStateTLBRENTRY
        io.DifftestBundle.DifftestCSRRegStateDMW0 := CSRMan.DifftestCSRRegStateDMW0
        io.DifftestBundle.DifftestCSRRegStateDMW1 := CSRMan.DifftestCSRRegStateDMW1

        val DifftestDelayBundle = out(Reg(new Bundle {
          // For Difftest Debug Usage
          /*
          input [ 7:0] index,
          input        valid,
          input [63:0] pc,
          input [31:0] instr,
          input        skip,
          input        is_TLBFILL,
          input [ 4:0] TLBFILL_index,
          input        is_CNTinst,
          input [63:0] timer_64_value,
          input        wen,
          input [ 7:0] wdest,
          input [63:0] wdata,
          input        csr_rstat,
          input [31:0] csr_data
           */
          val DifftestInstrCommitIndex = out Vec (out Bits (8 bits), config.rob.retireWidth)
          val DifftestInstrCommitValid = out Vec (out Bool (), config.rob.retireWidth)
          val DifftestInstrCommitPC = out Vec (out Bits (64 bits), config.rob.retireWidth)
          val DifftestInstrCommitInstr = out Vec (out Bits (32 bits), config.rob.retireWidth)
          val DifftestSkip = out Vec (out Bool (), config.rob.retireWidth)
          val DifftestIsTlbFill = out Vec (out Bool (), config.rob.retireWidth)
          val DifftestTlbFillIndex = out Vec (out Bits (5 bits), config.rob.retireWidth)
          val DifftestIsCount = out Vec (out Bool (), config.rob.retireWidth)
          val DifftestCount = out Vec (out Bits (64 bits), config.rob.retireWidth)
          val DifftestWen = out Vec (out Bool (), config.rob.retireWidth)
          val DifftestWdest = out Vec (out Bits (8 bits), config.rob.retireWidth)
          val DifftestWdata = out Vec (out Bits (64 bits), config.rob.retireWidth)
          val DifftestCsrRstat = out Vec (out Bool (), config.rob.retireWidth)
          val DifftestCsrData = out Vec (out Bits (32 bits), config.rob.retireWidth)

          /*
          input        excp_valid,
          input        eret,
          input [31:0] intrNo,
          input [31:0] cause,
          input [63:0] exceptionPC,
          input [31:0] exceptionInst
           * */
          val DifftestExcpEventExcpValid = out Bool ()
          val DifftestExcpEventEret = out Bool ()
          val DifftestExcpEventIntrNO = out Bits (32 bits)
          val DifftestExcpEventCause = out Bits (32 bits)
          val DifftestExcpEventEPC = out Bits (64 bits)
          val DifftestExcpEventInst = out Bits (32 bits)

          /*
           * DifftestStoreEvent
          input [ 7:0] valid,
          input [63:0] storePAddr,
          input [63:0] storeVAddr,
          input [63:0] storeData
           * */
          val DifftestStoreEventValid = out(Bits(8 bits))
          val DifftestStoreEventPAddr = out(Bits(64 bits))
          val DifftestStoreEventVAddr = out(Bits(64 bits))
          val DifftestStoreEventData = out(Bits(64 bits))

          /*
          DifftestLoadEvent
          input [ 7:0] valid,
          input [63:0] paddr,
          input [63:0] vaddr
           */
          val DifftestLoadEventValid = out(Bits(8 bits))
          val DifftestLoadEventPAddr = out(Bits(64 bits))
          val DifftestLoadEventVAddr = out(Bits(64 bits))
        }))

        io.DifftestDelayBundle.assignAllByName(DifftestDelayBundle)
        DifftestDelayBundle.assignSomeByName(io.DifftestBundle)

      } else {
        io.DifftestBundle.assignDontCare()
      }
    }

  }.setCompositeName(this)

  addPrePopTask { () =>
    Axi4Rename.Rename(io) // To make Axi4 signals match with loongchip
  }

  io.ws_valid := False
  io.rf_rdata := 0x0

}
