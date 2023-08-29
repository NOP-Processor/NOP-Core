package NOP.pipeline.core

import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.`enum`.{CacheOpType, CacheSelType, LoadStoreType, TLBOpType}
import NOP.pipeline._
import NOP.pipeline.decode._
import NOP.pipeline.fetch._
import NOP.pipeline.core._
import NOP.pipeline.priviledge._
import spinal.lib.fsm._

class CommitPlugin(config: MyCPUConfig)
    extends Plugin[MyCPUCore]
    with ExceptionCommit
    with TLBCommit
    with CacheCommit
    with BPUCommit
    with CSRCommit
    with ARFCommit
    with WaitCommit
    with StoreBufferCommit
    with CommitFlush {

  override val predUpdate = Flow(PredictUpdateBundle(config.frontend))

  private val retireWidth = config.rob.retireWidth
  private val decodeWidth = config.decode.decodeWidth

  // 向外的提交接口
  override val arfCommits = Vec(Flow(RegFileMappingEntryBundle(config.regFile)), retireWidth)

  // TODO: [NOP] Remove debug signals
  val DuncachedMask = out Bits (3 bits)

  override def build(pipeline: MyCPUCore): Unit = pipeline plug {
    val robFIFO = pipeline.service(classOf[ROBFIFOPlugin])
    val jumpInterface = pipeline.fetchPipeline.service(classOf[ProgramCounterPlugin]).backendJumpInterface

    // Insert into RENAME stage
    pipeline.RENAME plug new Area {
      import pipeline.RENAME._
      // dispatch to ROB at RENAME stage
      val decPacket = input(pipeline.decodePipeline.signals.DECODE_PACKET)
      val renameRecs = input(pipeline.decodePipeline.signals.RENAME_RECORDS)
      val robIdxs = insert(pipeline.decodePipeline.signals.ROB_INDEXES)
      val pushPorts = robFIFO.fifoIO.push
      for (i <- 0 until decodeWidth) {
        val valid = decPacket(i).valid
        val uop = decPacket(i).payload
        val rename = renameRecs(i)

        // Write to push ports of ROBFIFO
        val port = pushPorts(i)
        val entry = port.payload // ROBEntryBundle

        // ! Set ROBEntryBundle
        entry.info.uop.assignSomeByName(uop) // Assign MicroOp to ROBMicroOp
        entry.info.rename.assignAllByName(rename) // ROBRenameRecordBundle
        entry.info.frontendExc := uop.except.valid

        entry.state.complete := uop.needNotExecute
        entry.state.except.assignSomeByName(uop.except)
        entry.state.mispredict := !uop.branchLike && uop.predInfo.predictTaken
        entry.state.actualTaken := False

        port.valid := arbitration.isValidNotStuck && valid
        arbitration.haltItself setWhen (arbitration.isValid && valid && !port.ready)
        robIdxs(i) := robFIFO.pushPtr + i
      }
    }

    val retire = new Area {
      // retire asynchronously pops from ROB
      val popPorts = robFIFO.fifoIO.pop

      // ! First set default values for variables in CommitTraits
      // Exception Commit
      ertn := False
      // ARF Commit
      arfCommits.foreach(_.setIdle())
      // Commit Flush
      predUpdate.setIdle()
      commitStore := False
      doWait := False
      CSRWrite.setIdle()
      tlbOp := TLBOpType.NONE
      tlbInvASID := 0
      tlbInvVPPN := 0
      cacheOp.setIdle()

      val hasExcept = Bool()
      val linearRecover = Bool()

      // ! Calculate instructions that can be retired
      // 所有mask相与得到最终可以retire的指令
      val completeMask = B((0 until retireWidth).map { i =>
        // 左侧包括自己都complete
        popPorts.take(i + 1).map(_.state.complete).andR
      })
      val excMask = Bits(retireWidth bits) // exception
      val uniqueMask = Bits(retireWidth bits) // unique retire
      val recoverMask = Bits(retireWidth bits) // mispredict recover
      val uncachedMask = out Bits (retireWidth bits) // uncached load/store
      DuncachedMask := uncachedMask // TODO: [NOP] Remove debug signals

      excMask(0) := True
      uniqueMask(0) := True
      recoverMask(0) := True

      for (i <- 1 until retireWidth) {
        val port = popPorts(i)
        // 左侧包括自己没有非 delay slot 控制流转移，可以 commit
        excMask(i) := !popPorts
          .take(i + 1)
          .map { p => p.state.except.valid || linearRecover }
          .orR
        // 1~i没有要求在0口commit的指令
        // 这里unique retire只要求在0口commit，并不影响其它非unique指令在后面commit，
        // 因此控制流指令不能只依赖unique retire
        // load指令包含在了unique retire中，因此不需要另外判断uncached了
        uniqueMask(i) := !popPorts
          .slice(1, i + 1)
          .map(p => p.info.uop.uniqueRetire)
          .orR

        // If port 0 mispredict, then recoverMask(1) is False
        recoverMask(i) := !popPorts
          .slice(0, i)
          .map(p => p.state.mispredict)
          .orR
      }

      // readyMask: which instructions can be committed (pop out from ROB)
      val readyMask = completeMask & excMask & uniqueMask & recoverMask & uncachedMask

      val port0Commit = new Area {
        // port0特殊处理
        val port = popPorts(0)
        val entry = port.payload
        val uop = port.info.uop
        val fire = port.fire

        // 中断处理：中断被当作exception提交，则自然屏蔽所有指令性提交
        // exception分类在handler中进行，若本身就有exception，中断会被优先处理
        val intPending = pipeline.service(classOf[InterruptHandlerPlugin]).intPending
        val intInhibit = False // 用于在uncached开始执行后屏蔽中断的提交
        hasExcept := entry.state.except.valid || (intPending && !intInhibit)

        val mispredict = entry.state.mispredict

        // 需要flush到PC+4的情况：1. 指令本身改变影响处理器状态；2. 非branch被预测改变了控制流
        linearRecover := uop.flushState || (!uop.branchLike && mispredict)

        // 需要回滚处理器状态的情况：
        // 1. 分支：预测错误
        // 2. 异常
        // 3. 其它提交后需要flush流水线的指令
        val recoverState = fire && (hasExcept || mispredict || linearRecover)
        // 如果不需要等delay slot，则立即flush
        needFlush := recoverState
        robFIFO.fifoIO.flush := needFlush // flush周期同时pop

        // mispredict仍然会提交分支指令和delay slot，所以要先更新ARF，再回滚PRF
        // flush的下一个周期不会pop，所以没有关系
        recoverPRF := regFlush // regFlush 是 needFlush 延迟一个周期

        val jumpTarget = U(0, 32 bits)

        // clear frontend pipelines
        pipeline.fetchPipeline.stages.last.arbitration.flushIt setWhen needFlush
        pipeline.decodePipeline.stages.last.arbitration.flushIt setWhen needFlush

        // exception永远从0口unique发出
        except.valid := fire && hasExcept
        except.payload := entry.state.except.payload
        // 前端异常的badVA必然是pc
        when(entry.info.frontendExc) { except.badVA := entry.info.uop.pc }
        epc := uop.pc

        when(fire & !hasExcept) {
          // 所有指令性的commit需要在没有except的时候发出

          cacheOp.valid := uop.operateCache
          // 复用badVA，如果没有异常的时候badVA填cache需要的物理地址
          cacheOp.payload.addr := entry.state.except.badVA
          cacheOp.payload.op assignFromBits entry.state.intResult(0, CacheOpType.None.asBits.getWidth bits).asBits
          cacheOp.payload.sel assignFromBits entry.state
            .intResult(CacheOpType.None.asBits.getWidth, CacheSelType.None.asBits.getWidth bits)
            .asBits

          doWait := uop.isWait
          tlbOp := uop.tlbOp
          tlbInvASID := entry.state.intResult(9 downto 0).asBits
          tlbInvVPPN := entry.state.intResult(28 downto 10).asBits

          when(uop.writeCSR) {
            CSRWrite.valid := True
            CSRWrite.payload.data := entry.state.intResult.asBits
            CSRWrite.payload.addr := uop.inst(23 downto 10).asUInt
          }

          ertn := uop.isErtn

          commitStore := uop.isStore

          val excHandler = pipeline.service(classOf[ExceptionHandlerPlugin])
          when(uop.isLL) {
            excHandler.LLBCTL_LLBIT := True
          }

          when(uop.isSC) {
            when(excHandler.LLBCTL_LLBIT) {
              excHandler.LLBCTL_LLBIT := False
            } otherwise {
              commitStore := False
            }
          }

          // ! BTB Pred Update
          predUpdate.valid := uop.branchLike || uop.predInfo.predictBranch
          predUpdate.payload.predInfo := uop.predInfo
          predUpdate.payload.predRecover := uop.predRecover
          predUpdate.payload.branchLike := uop.isBranch || uop.isJump || uop.isJR // jump is a always true branch for btb
          // returns also needed to record in btb for ras to predict correctly.
          predUpdate.payload.isTaken := (entry.state.mispredict ^ uop.predInfo.predictTaken) || uop.isJump || uop.isJR // note that jump and jr is always taken...
          // 'jr ra'
          predUpdate.payload.isRet := B"01001100000000000000000000100000" === uop.inst // 0x4c000020
          // Call: JIRL with RA linkage and BL
          predUpdate.payload.isCall := (uop.isJR && uop.inst(4 downto 0) === B"00001") || (uop.isJump && uop.inst(26))
          predUpdate.payload.mispredict := entry.state.mispredict
          predUpdate.payload.pc := uop.pc
          predUpdate.payload.target := entry.state.intResult

          when(uop.isJump || uop.isJR) {
            jumpTarget := entry.state.intResult // when is Jump, go to actual target
          } otherwise {
            jumpTarget := Mux(
              entry.state.actualTaken,
              entry.state.intResult,
              uop.pc + 4
            ) // when branch, decide recover or not based on predictTaken
          }

          when(linearRecover && !uop.isErtn) {
            // 这些是本身不改变控制流，但因为flush而需要改变控制流
            jumpInterface.valid := True
            // [NOP] 被改成了直接用 pc + 4
            jumpInterface.payload := uop.pc + 4
          }

          when(entry.state.mispredict && uop.branchLike) {
            jumpInterface.valid := True
            jumpInterface.payload := jumpTarget
          }

          // 以上优先级是重要的。Branch本身的跳转目标优先于delay slot中的mispredict（一定非branch）。
        }

        val uncachedProcess = new Area {
          // 处理uncached load的提交问题
          // uncached load要等待store buffer将其发射并执行到WB阶段才能提交
          // 避免出现寄存器已经被重分配出去的问题
          val isUncachedUOP = uop.isLoad && entry.state.lsuUncached
          val fsm = new StateMachine {
            disableAutoStart()
            setEntry(stateBoot)
            val execute = new State

            uncachedMask.setAll()
            stateBoot.whenIsActive {
              when(port.valid && isUncachedUOP && entry.state.complete && !hasExcept) {
                // 不提交，等待store buffer执行
                uncachedMask := 0
                commitStore := True
                goto(execute)
              }
            }

            execute.whenIsActive {
              intInhibit := True
              uncachedMask := 0
              val memWB = pipeline.memPipeline.WB
              val wbSTD = memWB.input(pipeline.memPipeline.signals.STD_SLOT)
              val isUncachedWB = wbSTD.valid && !wbSTD.isStore
              // uncached load执行完成
              when(memWB.arbitration.notStuck && isUncachedWB) {
                // FIXME: 需要保证此周期一定提交且不触发异常
                uncachedMask.setAll()
                goto(stateBoot)
              }
            }
          }
        }
      }

      for (i <- 0 until retireWidth) {
        val port = popPorts(i)
        val uop = port.info.uop
        val rename = port.info.rename
        port.ready := readyMask(i)
        when(port.fire & !hasExcept) {
          // 所有指令性的commit需要在没有except的时候发出
          arfCommits(i).valid := uop.doRegWrite
          arfCommits(i).payload.addr := uop.wbAddr
          arfCommits(i).payload.prevAddr := rename.wPrevReg
          arfCommits(i).payload.prfAddr := rename.wReg
        }
      }

    }

    retire
  }
}
