package NOP.pipeline.mem

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi.Axi4

import NOP._
import NOP.blackbox.mem._
import NOP.builder._
import NOP.utils._
import NOP.pipeline._
import NOP.pipeline.fetch._
import NOP.pipeline.core._
import NOP.constants.enum._

class DCachePlugin(config: MyCPUConfig) extends Plugin[MemPipeline] {
  private val dcache = config.dcache
  private val prfAddrWidth = config.regFile.prfAddrWidth

  val dBus = Axi4(config.axiConfig).setIdle() // DCache的AXI总线
  dBus.b.ready.allowOverride := True

  // valid有单index清空的可能，多写口
  val valids = Vec(Vec(RegInit(False), dcache.ways), dcache.sets)
  val dirtyBitsManager = new DirtyBitsManager(config.dcache)
  // 虽然d-cache形式上组织成例如32B一行，但是实际每次读写都只需要一个word，因此物理上这么组织最省面积和延迟
  val dataRAMs = Seq.fill(dcache.ways)(
    new SDPRAM(BWord(), dcache.lineWords * dcache.sets, true, useByteEnable = true)
  )
  val infoRAM = new SDPRAM(CacheLineInfo(dcache), dcache.sets, false)

  private object DCACHE_VALIDS extends Stageable(valids.dataType())
  private object DCACHE_DIRTY extends Stageable(dirtyBitsManager.dirtyBits.dataType())
  private object DCACHE_INFO extends Stageable(CacheLineInfo(dcache))
  private object TAG_MATCHES extends Stageable(Bits(dcache.ways bits))

  val writebackIdle = False

  override def build(pipeline: MemPipeline): Unit = pipeline plug new Area {
    import pipeline.signals._
    val rPort = infoRAM.io.read
    val dataRs = Vec(dataRAMs.map(_.io.read))
    // 为了避免MEM1取不到MEM2写的cache，做refetch重取
    val doRefetch = False
    val refetchValid = doRefetch
    val wordAddrRange = dcache.indexRange.high downto 2
    val mem1MemAddr = UWord()

    pipeline.MEMADDR plug new Area {
      import pipeline.MEMADDR._
      val std = input(STD_SLOT)
      val memAddr = Mux(std.valid, std.addr, output(MEMORY_ADDRESS))
      val rValid = arbitration.notStuck || refetchValid
      val rAddr = Mux(refetchValid, mem1MemAddr, memAddr)
      // 读tag按照index找行
      rPort.cmd.valid := rValid
      rPort.cmd.payload := rAddr(dcache.indexRange)
      // 读data应当直接找word
      dataRs.foreach { p =>
        p.cmd.valid := rValid
        p.cmd.payload := rAddr(wordAddrRange)
      }
    }

    pipeline.MEM1 plug new Area {
      import pipeline.MEM1._
      arbitration.haltItself setWhen refetchValid
      val std = input(STD_SLOT)
      val memAddr = Mux(std.valid, std.addr, input(MEMORY_ADDRESS))
      mem1MemAddr := memAddr
      // 读valid也插进流水线
      insert(DCACHE_VALIDS) := valids(memAddr(dcache.indexRange))
      dirtyBitsManager.io.readCmd := memAddr(dcache.indexRange)
      insert(DCACHE_DIRTY) := dirtyBitsManager.io.readRsp
      insert(DCACHE_INFO) := rPort.rsp
      val cachePhysAddr = Mux(std.valid, std.addr, input(MEMORY_ADDRESS_PHYSICAL))
      for (i <- 0 until dcache.ways) {
        insert(TAG_MATCHES)(i) := input(DCACHE_INFO).tags(i) === cachePhysAddr(dcache.tagRange)
      }

      // 拆issue slot
      val issSlot = input(ISSUE_SLOT)
      insert(ROB_IDX) := issSlot.robIdx
      insert(IS_LOAD) := issSlot.uop.isLoad
      insert(IS_STORE) := issSlot.uop.isStore
      insert(WRITE_REG).valid := issSlot.uop.doRegWrite
      insert(WRITE_REG).payload := issSlot.wReg
      insert(LOAD_STORE_TYPE) := issSlot.uop.lsType
      for (i <- 0 until issSlot.rRegs.size) {
        insert(READ_REGS)(i) := issSlot.rRegs(i).payload
      }
    }

    pipeline.MEM2 plug new Area {
      import pipeline.MEM2._

      val EXC_SIGNALS = pipeline.service(classOf[ExceptionMuxPlugin[pipeline.type]]).ExceptionSignals

      val storeBuffer = pipeline.service(classOf[StoreBufferPlugin])
      val addrCached = input(ADDRESS_CACHED)
      // 注意这3者存在同时不满足的情况，此时为cache类指令
      // STD = store data
      val std = input(STD_SLOT)
      val isSTD = std.valid && std.isStore && std.isCached
      val isLoad = input(IS_LOAD)
      // STA = store address
      val isSTA = input(IS_STORE)
      val isCACHE = !input(IS_LOAD) && !input(IS_STORE)
      val noExcept = !input(EXC_SIGNALS.EXCEPTION_OCCURRED)

      // uncached
      val isLDU = std.valid && !std.isStore
      val isSTU = std.valid && std.isStore && !std.isCached

      // 如果load发现uncached或者exception，那么跳过所有cache处理
      val reqValid = (arbitration.isValidOnEntry && isLoad && addrCached && noExcept) || isSTD
      val reqCommit = reqValid && !arbitration.isStuck
      val virtAddr = input(MEMORY_ADDRESS)
      val physAddr = input(MEMORY_ADDRESS_PHYSICAL)
      val memWData = input(MEMORY_WRITE_DATA)
      val memBE = input(MEMORY_BE)
      val cachePhysAddr = Mux(std.valid, std.addr, input(MEMORY_ADDRESS_PHYSICAL))
      val idx = cachePhysAddr(dcache.indexRange)
      val tag = cachePhysAddr(dcache.tagRange) // physical tag
      val setValids = input(DCACHE_VALIDS)
      val wPort = infoRAM.io.write.setIdle()
      val dataWs = Vec(dataRAMs.map(_.io.write.setIdle()))
      val dataMasks = Vec(dataRAMs.map(_.io.writeMask.setAll()))

      // STA忽略cache，但是无异常且cached时要在MEM2写入store buffer
      val storeBufferPush = storeBuffer.queueIO.pushPort
      val storeBufferPushValid =
        arbitration.isValid && noExcept && (isSTA || (isLoad && !addrCached))
      // 扩展后写入store buffer的：cached store, uncached load, uncached store
      storeBufferPush.valid := storeBufferPushValid && arbitration.notStuck
      storeBufferPush.addr := physAddr
      // store buffer永远放移好位的
      storeBufferPush.data := input(MEMORY_WRITE_DATA)
      storeBufferPush.be := input(MEMORY_BE)
      storeBufferPush.retired := False
      storeBufferPush.isStore := isSTA
      storeBufferPush.isCached := addrCached
      storeBufferPush.wReg := input(WRITE_REG)
      storeBufferPush.lsType := input(LOAD_STORE_TYPE)
      storeBufferPush.robIdx := input(ROB_IDX)
      // store buffer不应push不进去，否则memory流水也执行不了STD，就死锁了
      arbitration.haltItself setWhen (storeBufferPushValid && !storeBufferPush.ready)

      // cache查询
      val hits = for (i <- 0 until setValids.size) yield {
        setValids(i) && input(TAG_MATCHES)(i)
      }
      val hit = hits.orR
      val hitData = MuxOH(hits, dataRAMs.map(_.io.read.rsp))
      val hitWay = hits(1).asUInt
      val replaceWay = input(DCACHE_INFO).lru.asUInt

      // CACHE指令相关信息
      val wayCACHE = CombInit(hitWay)
      val cacheOp = input(ISSUE_SLOT).uop.cacheOp
      val cacheSel = input(ISSUE_SLOT).uop.cacheSel

      // 存储当前被填充的word，避免MEM2重取
      val storedWord = Reg(BWord())
      dirtyBitsManager.io.writeCmd.setIdle()

      when(reqCommit && hit) {
        // 命中，提交LRU修改
        assert(dcache.ways == 2)
        val newInfo = input(DCACHE_INFO).copy()
        newInfo.tags := input(DCACHE_INFO).tags
        // hit 0, set 1
        newInfo.lru(0) := hits(0)
        wPort.valid.set()
        wPort.payload.address := idx
        wPort.payload.data := newInfo
        when(isSTD) {
          // STD提交写cache
          dataWs(hitWay).valid := True
          dataWs(hitWay).payload.address := std.addr(wordAddrRange)
          dataWs(hitWay).payload.data := std.data
          dataMasks(hitWay) := std.be
          dirtyBitsManager.io.writeCmd.valid := True
          dirtyBitsManager.io.writeCmd.idx := idx
          dirtyBitsManager.io.writeCmd.way := hitWay
          dirtyBitsManager.io.writeCmd.data := True
          // 并不需要重取，MEM1自然还会hit，数据可以从store buffer前传出来
          // doRefetch := True
        }
      }

      // 触发将脏块写回内存的状态机
      val triggerWriteback = False
      // 由于修复Uncached触发Writeback
      val triggerWritebackFixUncache = False
      // 由于CACHE指令触发Writeback
      val triggerWritebackCACHE = False
      // 写回块取cache需要早于读块写cache
      val lockCache = False
      // 为了write dirty line，提前fetch出来
      val dirtyLine = Reg(Vec(BWord(), dcache.lineWords))
      // 仍然是同步写，但是与读内存并行了
      val dirtyLineWritebackFSM = new StateMachine {
        disableAutoStart()
        setEntry(stateBoot)
        val fetchCache = new State
        val waitAW = new State
        val writeMem = new State
        val waitB = new State

        // val wayIdx = ~input(DCACHE_INFO).lru.asUInt
        val wayIdx = RegInit(U(0, log2Up(config.dcache.ways) bits)) // Change to register
        val rspId = Counter(dcache.lineWords)
        val rspIdDelayed = Delay(rspId.value, 2)

        stateBoot.whenIsActive {
          writebackIdle := True
          when(triggerWriteback || triggerWritebackFixUncache || triggerWritebackCACHE) {
            rspId.clear()
            goto(fetchCache)
            when(triggerWriteback) {
              wayIdx := replaceWay
            } elsewhen (triggerWritebackFixUncache) {
              wayIdx := hitWay
            } otherwise {
              // triggerWritebackCACHE
              wayIdx := wayCACHE
            }
          }
        }
        fetchCache.whenIsActive {
          arbitration.haltItself.set()
          lockCache := True
          rspId.increment()
          dataRs.foreach(_.cmd.push(idx @@ rspId))
          dirtyLine(rspIdDelayed) := Vec(dataRAMs.map(_.io.read.rsp))(wayIdx)
          when(rspIdDelayed === dcache.lineWords - 1) {
            goto(waitAW)
          }
        }
        // write-back dirty block
        waitAW.whenIsActive {
          arbitration.haltItself.set()
          val aw = dBus.aw
          aw.valid := True
          aw.payload.id := 1
          aw.payload.addr := input(DCACHE_INFO).tags(wayIdx) @@ idx @@ U(0, dcache.offsetWidth bits)
          aw.payload.len := dcache.lineWords - 1 // burst len
          aw.payload.size := 2
          aw.payload.burst := 1 // burst type = INCR
          aw.payload.lock := 0
          aw.payload.cache := 0
          if (config.axiConfig.useQos) aw.payload.qos := 0
          aw.payload.prot := 0
          when(aw.ready) {
            rspId.clear()
            goto(writeMem)
          }
        }
        writeMem.whenIsActive {
          arbitration.haltItself.set()
          val w = dBus.w
          w.valid := True
          w.data := dirtyLine(rspId)
          w.strb.setAll()
          w.last := rspId.willOverflowIfInc
          when(w.ready) {
            rspId.increment()
            when(w.last) {
              goto(waitB)
            }
          }
        }

        waitB.whenIsActive {
          arbitration.haltItself.set()
          val b = dBus.b
          when(b.valid) {
            goto(stateBoot)
          }
        }
      }

      // 处理Cache命中但是地址变为uncached时的刷新逻辑
      // NOTE:
      // 1. 这个东西不会和下面的uncached store冲突，因为STA必然在STD之前，
      // 所以STD不可能触发这段逻辑
      // 2. 逻辑是isLDU或者isSTU的时候，触发dirty writeback, 写完之后，置对应cache非法
      // 3. 握手：finishWriteback
      val fixUncacheFSM = new StateMachine {
        disableAutoStart()
        setEntry(stateBoot)
        val waitWriteback = new State

        val dirty = setValids(hitWay) && input(DCACHE_DIRTY)(hitWay)

        stateBoot.whenIsActive {
          when((isLDU || isSTU) && hit) {
            arbitration.haltItself := True
            triggerWritebackFixUncache := dirty
            goto(waitWriteback)
          }
        }

        waitWriteback.whenIsActive {
          triggerWritebackFixUncache := False
          // 每条都从state boot开始
          when(arbitration.notStuck) {
            valids(idx)(hitWay) := False // invalidate the cache
            doRefetch := True // valid状态改了，需要refetch
            goto(stateBoot)
          }
        }
      }

      // 处理CACHE指令可能引发的写回操作
      val CACHEFSM = new StateMachine {
        disableAutoStart()
        setEntry(stateBoot)
        val waitWriteBack = new State
        val checkWay = new State
        val writebackWay = new State

        val finishAllWay = Reg(Bool())
        val wayCounter = Reg(UInt(log2Up(dcache.ways) bits))
        val indexCounter = Reg(UInt(log2Up(dcache.sets) bits))

        stateBoot.whenIsActive {
          when(arbitration.isValidOnEntry && isCACHE && cacheSel === CacheSelType.DCache) {
            switch(input(ISSUE_SLOT).uop.cacheOp) {
              import CacheOpType._
              is(IndexInvalidate) {
                arbitration.haltItself.set()
                finishAllWay := False
                wayCounter := 0
                goto(checkWay)
              }
              is(HitInvalidate) {
                arbitration.haltItself.set()
                finishAllWay := False
                wayCounter := 0
                goto(checkWay)
              }
              is(StoreTag) {
                for (i <- 0 until dcache.sets) {
                  for (j <- 0 until (dcache.ways)) {
                    valids(i)(j) := False
                  }
                }
              }
            }
          }
        }

        checkWay.whenIsActive {
          wayCACHE := wayCounter
          when(finishAllWay) {
            when(arbitration.notStuck) {
              doRefetch.set()
              goto(stateBoot)
            }
          } otherwise {
            arbitration.haltItself.set()
            // 检查此路是否dirty，若dirty则写回。无论是否写回均转状态
            triggerWritebackCACHE := setValids(wayCounter) && input(DCACHE_DIRTY)(wayCounter)
            when(writebackIdle) {
              goto(writebackWay)
            }
          }
        }
        writebackWay.whenIsActive {
          arbitration.haltItself.set()
          when(writebackIdle) {
            // 清除此路valid，转下一路
            valids(idx)(wayCounter).clear()
            goto(checkWay)
            when(wayCounter === dcache.ways - 1) {
              finishAllWay := True
            }
            wayCounter := wayCounter + 1
          }
        }

        // Hit Writeback Invalidate
        waitWriteBack.whenIsActive {
          when(arbitration.notStuck) {
            valids(idx)(wayCACHE).clear()
            doRefetch.set()
            goto(stateBoot)
          }
        }
      }

      // 解决cache miss
      // note. 向cache填充的不一定是目前的memory state，因为有store buffer未完成写的部分。
      //       但是STD会将这二者都更新，并且更新前load也可以从store buffer读到最新的(包括speculative的)数据。
      val cacheRefillFSM = new StateMachine {
        // cached load
        val waitAXI = new State
        val readMem = new State
        val commit = new State
        val finish = new State
        // uncached load
        val waitAXIU = new State
        val readMemU = new State
        val finishU = new State
        disableAutoStart()
        setEntry(stateBoot)
        val rspId = Counter(dcache.lineWords)
        // 反正寄存也不增加周期，还改善时序
        val refillValid = RegNext(dBus.r.fire)
        val regBusWord = RegNext(dBus.r.payload.data)
        val refillWord = CombInit(regBusWord)
        // STD直接往cache填入更新了本次写的数据
        for (i <- 0 until 4)
          when(isSTD && rspId === std.addr(dcache.wordOffsetRange) && std.be(i)) {
            refillWord(i * 8, 8 bits) := std.data(i * 8, 8 bits)
          }
        // 根据LRU选择一路
        val dirty = setValids(replaceWay) && input(DCACHE_DIRTY)(replaceWay)

        // 填充stored word
        when(refillValid && rspId === cachePhysAddr(dcache.wordOffsetRange)) {
          storedWord := refillWord
        }

        stateBoot.whenIsActive {
          // write-back write allocate, STD需要refill cache
          when(reqValid && !hit) {
            arbitration.haltItself.set()
            rspId.clear()
            triggerWriteback := dirty
            goto(waitAXI)
          }
          when(isLDU) {
            arbitration.haltItself.set()
            goto(waitAXIU)
          }
        }

        // cached load
        waitAXI.whenIsActive {
          arbitration.haltItself.set()
          val ar = dBus.ar
          ar.payload.id := 1
          // 抹去offset
          ar.payload.addr := cachePhysAddr(31 downto dcache.offsetWidth) @@
            U(0, dcache.offsetWidth bits)
          ar.payload.len := dcache.lineWords - 1 // burst len
          ar.payload.size := 2 // burst size = 4Bytes = 32 bits
          ar.payload.burst := 1 // burst type = INCR
          ar.payload.lock := 0 // normal access
          ar.payload.cache := 0 // device non-bufferable
          if (config.axiConfig.useQos) ar.payload.qos := 0 // no QoS scheme
          ar.payload.prot := 0 // secure and normal(non-priviledged)
          ar.valid := True
          when(ar.ready) {
            rspId.clear()
            goto(readMem)
          }
        }

        readMem.whenIsActive {
          arbitration.haltItself.set()
          val r = dBus.r
          r.ready := !lockCache
          when(refillValid) {
            dataWs(replaceWay).valid := True
            dataWs(replaceWay).payload.address := idx @@ rspId
            dataWs(replaceWay).payload.data := refillWord
            rspId.increment()
          }
          when(r.fire && r.payload.last) { goto(commit) }
        }

        commit.whenIsActive {
          arbitration.haltItself.set()
          // 对data写入最后一个word
          dataWs(replaceWay).valid := True
          dataWs(replaceWay).payload.address := idx @@ rspId
          dataWs(replaceWay).payload.data := refillWord
          // 更新info
          assert(dcache.ways == 2)
          val newInfo = input(DCACHE_INFO).copy()
          // 其它路tag保持
          newInfo.tags := input(DCACHE_INFO).tags
          // 选择的一路写入新tag
          newInfo.tags(replaceWay) := tag
          // 更新LRU
          newInfo.lru := ~input(DCACHE_INFO).lru
          dirtyBitsManager.io.writeCmd.valid := True
          dirtyBitsManager.io.writeCmd.idx := idx
          dirtyBitsManager.io.writeCmd.way := replaceWay
          dirtyBitsManager.io.writeCmd.data := isSTD
          // 设置valid
          valids(idx)(replaceWay).set()
          // 写入Mem
          wPort.valid.set()
          wPort.payload.address := idx
          wPort.payload.data := newInfo
          rspId.clear()
          goto(finish)
        }

        finish.whenIsActive {
          // 每一条都从state boot开始
          when(!arbitration.isStuck) {
            doRefetch := True
            goto(stateBoot)
          }
        }

        // uncached load
        val udBus = pipeline.service(classOf[UncachedAccessPlugin]).udBus
        val uncachedStoreHandshake = pipeline.service(classOf[UncachedAccessPlugin]).uncachedStoreHandshake
        waitAXIU.whenIsActive {
          arbitration.haltItself.set()
          val ar = udBus.ar
          ar.payload.id := 2
          ar.payload.addr := std.addr
          ar.payload.len := 0 // burst len
          ar.payload.size := LoadStoreType.toAxiSize(std.payload.lsType)
          ar.payload.burst := 1 // burst type = INCR
          ar.payload.lock := 0 // normal access
          ar.payload.cache := 0 // device non-bufferable
          if (config.axiConfig.useQos) ar.payload.qos := 0 // no QoS scheme
          ar.payload.prot := 0 // secure and normal(non-priviledged)
          // uncached load要等到脏行写完再进行
          // 尽量让uncached load也不要与正在进行中的uncached store重叠
          ar.valid := uncachedStoreHandshake.ready && pipeline
            .service(classOf[DCachePlugin])
            .writebackIdle
          when(ar.fire) { goto(readMemU) }
        }

        readMemU.whenIsActive {
          arbitration.haltItself.set()
          val r = udBus.r
          r.ready.set()
          when(r.valid && r.payload.last) {
            storedWord := r.payload.data
            goto(finishU)
          }
        }

        finishU.whenIsActive {
          // 每一条都从state boot开始
          when(!arbitration.isStuck) {
            goto(stateBoot)
          }
        }
      }

      // ! Load Logic: assemble MEMORY_READ_DATA
      val cacheData = Mux(hit, hitData, storedWord)
      val memRData = insert(MEMORY_READ_DATA)
      memRData := cacheData

      // load std前传
      def stdValid(stage: Stage) = {
        val std = stage.input(STD_SLOT)
        std.valid && std.isStore && std.isCached
      }
      pipeline.stages.reverse
        .filter(stage => stage.ne(pipeline.ISS))
        .foreach(stage => {
          when(
            physAddr(2, 30 bits) === stage.input(STD_SLOT).addr(2, 30 bits) && // same addr
              stdValid(stage) // is STD
          ) {
            for (i <- 0 until 4) when(stage.input(STD_SLOT).be(i)) {
              memRData(i * 8, 8 bits) := stage.input(STD_SLOT).data(i * 8, 8 bits);
            }
          }
        })

      // load查询store buffer
      storeBuffer.query.addr := physAddr
      for (i <- 0 until 4) when(storeBuffer.query.be(i)) {
        memRData(i * 8, 8 bits) := storeBuffer.query.data(i * 8, 8 bits)
      }

      // LDU必然选stored word
      when(isLDU) { memRData := storedWord }
    }
  }
}
