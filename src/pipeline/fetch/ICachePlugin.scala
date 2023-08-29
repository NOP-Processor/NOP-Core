package NOP.pipeline.fetch

import NOP.blackbox.mem._
import NOP.pipeline._
import NOP.builder._
import NOP.utils._
import NOP._
import NOP.constants.`enum`.{CacheOpType, CacheSelType}
import NOP.pipeline.core.{CommitPlugin, ExceptionMuxPlugin}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

final case class CacheLineInfo(config: CacheBasicConfig, useDirty: Boolean = false) extends Bundle {
  val lru = Bits(log2Up(config.ways) bits)
  val tags = Vec(UInt(config.tagRange.size bits), config.ways)
  val dirtyBits = useDirty generate Bits(config.ways bits)
}

final case class CacheOperation() extends Bundle {
  val addr = UWord()
  val sel = CacheSelType()
  val op = CacheOpType()
}

class ICachePlugin(config: MyCPUConfig) extends Plugin[FetchPipeline] {
  private val frontend = config.frontend
  private val icache = config.frontend.icache

  // * Cache Valid/DATA/CacheLineInfo area
  // valid有单index清空的可能，多写口
  val valids = Vec(Vec(RegInit(False), icache.ways), icache.sets) // valids[sets][ways]
  val dataRAMs =
    Seq.fill(icache.ways)(
      new SDPRAM(Vec(BWord(), icache.lineWords), icache.sets, false, useByteEnable = true)
    ) // dataRAMs[ways][sets]
  val infoRAM = new SDPRAM(CacheLineInfo(icache), icache.sets, false) // CacheLineInfo[sets]

  // * Cache spans from IF1 to IF2. The following signals are stageable.
  private object ICACHE_VALIDS extends Stageable(valids.dataType()) // Bool[sets][ways]
  // BWord[ways][fetchWidth]
  private object ICACHE_RSPS extends Stageable(Vec(Vec(BWord(), frontend.fetchWidth), icache.ways))
  private object ICACHE_INFO extends Stageable(CacheLineInfo(icache)) // CacheLineInfo

  // * Cache Interface
  // iBus本身一定是read only，但对外可以转Axi4
  val iBus = Axi4ReadOnly(config.axiConfig).setIdle()

  override def build(pipeline: FetchPipeline): Unit = pipeline plug new Area {
    // * Plugin Area into FetchPipeline
    import pipeline._
    // I-cache重取
    val doRefetch = False
    // IF1为气泡的时候跳过refetch，省一个周期
    val refetchValid = pipeline.IF1.arbitration.isValid && doRefetch
    // 读tag按照index找行
    val nextPC = pipeline.service(classOf[ProgramCounterPlugin]).nextPC
    val rValid = !pipeline.IF1.arbitration.isStuck || refetchValid
    // If refetch, read from PC, else read from nextPC

    // Set infoRAM readPort
    val rAddr = Mux(refetchValid, pipeline.IF1.input(pipeline.signals.PC), nextPC)
    val rPort = infoRAM.io.read
    rPort.cmd.valid := rValid
    rPort.cmd.payload := rAddr(icache.indexRange)

    // Set dataRAM readPort
    val dataRs = Vec(dataRAMs.map(_.io.read))
    dataRs.foreach { p =>
      p.cmd.valid := rValid
      p.cmd.payload := rAddr(icache.indexRange)
    }

    // * Plugin Area into IF1
    IF1 plug new Area {
      import IF1._
      arbitration.haltItself setWhen refetchValid
      val pc = input(pipeline.signals.PC)
      // 读valid也插进流水线
      insert(ICACHE_VALIDS) := valids(pc(icache.indexRange))
      for (i <- 0 until icache.ways) {
        for (j <- 0 until frontend.fetchWidth) {
          // ICACHE_RSPS: BWord[ways][fetchWidth]; dataRAMs[ways][sets]
          insert(ICACHE_RSPS)(i)(j) := dataRAMs(i).io.read.rsp(pc(icache.wordOffsetRange) + j)
        }
      }
      insert(ICACHE_INFO) := rPort.rsp
    }

    // * Plugin Area into IF2
    IF2 plug new Area {
      import IF2._

      val fetchExcHandler = pipeline.service(classOf[ExceptionMuxPlugin[FetchPipeline]])
      val reqValid = arbitration.isValidOnEntry && !output(fetchExcHandler.ExceptionSignals.EXCEPTION_OCCURRED)
      val reqCommit = reqValid && !arbitration.isStuck
      val virtPC = input(pipeline.signals.PC)
      val physPC = input(pipeline.signals.PC_PHYSICAL)
      // PC在cache line中的位置，用于计算fetch word是否有效
      val pcWordOffset = virtPC(icache.wordOffsetRange)

      // Virtual Index, Physical Tag
      val idx = virtPC(icache.indexRange)
      val tag = physPC(icache.tagRange) // physical tag
      val setValids = input(ICACHE_VALIDS)

      // ! Prepare writePorts for replacing
      val wPort = infoRAM.io.write.setIdle()
      val dataWs = Vec(dataRAMs.map(_.io.write.setIdle()))
      val dataMasks = Vec(dataRAMs.map(_.io.writeMask.clearAll().subdivideIn(4 bits, true)))

      // cache查询
      val hits = setValids.zip(input(ICACHE_INFO).tags).map { case (valid, t) =>
        valid && t === tag
      }
      val hit = hits.orR
      val hitData = MuxOH(hits, input(ICACHE_RSPS))

      // 存储当前被填充的packet，避免IF2重取
      val storedPacket = Vec(Reg(BWord()), frontend.fetchWidth)

      when(reqCommit && hit) {
        // 命中，提交LRU修改
        assert(icache.ways == 2)
        val newInfo = input(ICACHE_INFO).copy()
        newInfo.tags := input(ICACHE_INFO).tags
        // Hit 0, then set 1, means replace way 1 if miss next time
        newInfo.lru(0) := hits(0)
        wPort.valid.set()
        wPort.payload.address := idx
        wPort.payload.data := newInfo
      }

      // 解决cache miss
      val cacheRefillFSM = new StateMachine {
        val waitAXI = new State
        val readMem = new State
        val commit = new State
        val finish = new State

        disableAutoStart()
        setEntry(stateBoot)

        val rspId = Counter(icache.lineWords)

        // 反正寄存也不增加周期，还改善时序
        val refillValid = RegNext(iBus.r.fire)
        val refillWord = RegNext(iBus.r.payload.data)
        // 根据LRU选择一路
        val replaceWay = input(ICACHE_INFO).lru.asUInt
        // 填充stored packet
        for (i <- 0 until frontend.fetchWidth) {
          when(refillValid && rspId === pcWordOffset + i) {
            storedPacket(i) := refillWord
          }
        }

        stateBoot.whenIsActive {
          when(reqValid && !hit) {
            arbitration.haltItself.set()
            goto(waitAXI)
          }
        }

        waitAXI.whenIsActive {
          arbitration.haltItself.set()
          val ar = iBus.ar
          ar.payload.id := 0
          // 抹去offset
          ar.payload.addr := physPC(31 downto icache.offsetWidth) @@ U(0, icache.offsetWidth bits)
          ar.payload.len := icache.lineWords - 1 // burst len
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
          val r = iBus.r
          r.ready.set()
          when(refillValid) {
            dataWs(replaceWay).valid := True
            dataWs(replaceWay).payload.address := idx
            dataWs(replaceWay).payload.data.foreach(_ := refillWord)
            dataMasks(replaceWay)(rspId).setAll()
            rspId.increment()
          }
          when(r.valid && r.payload.last)(goto(commit))
        }

        commit.whenIsActive {
          arbitration.haltItself.set()
          // 对data写入最后一个word
          dataWs(replaceWay).valid := True
          dataWs(replaceWay).payload.address := idx
          dataWs(replaceWay).payload.data.foreach(_ := refillWord)
          dataMasks(replaceWay)(rspId).setAll()
          // 更新info
          assert(icache.ways == 2)
          val newInfo = input(ICACHE_INFO).copy()
          // 其它路tag保持
          newInfo.tags := input(ICACHE_INFO).tags
          // 选择的一路写入新tag
          newInfo.tags(replaceWay) := tag
          // 更新LRU
          newInfo.lru := ~input(ICACHE_INFO).lru
          // 设置valid
          valids(idx)(replaceWay).set()
          // 写入Mem
          wPort.valid.set()
          wPort.payload.address := idx
          wPort.payload.data := newInfo
          goto(finish)
        }

        finish.whenIsActive {
          // 避免IF1中指令取不到新写的cache，重取
          // 每一条都从state boot开始
          when(!arbitration.isStuck) {
            doRefetch := !IF1.arbitration.isFlushed
            goto(stateBoot)
          }
        }
      }

      val fetchPacket = insert(pipeline.signals.FETCH_PACKET)
      fetchPacket.pc := virtPC
      fetchPacket.except.valid := output(fetchExcHandler.ExceptionSignals.EXCEPTION_OCCURRED)
      fetchPacket.except.payload.code := output(fetchExcHandler.ExceptionSignals.EXCEPTION_ECODE)
      fetchPacket.except.payload.subcode := output(fetchExcHandler.ExceptionSignals.EXCEPTION_ESUBCODE)
      fetchPacket.except.payload.isTLBRefill := output(pipeline.signals.IS_TLB_REFILL)

      for (i <- 0 until frontend.fetchWidth) {
        val fetchWord = fetchPacket.insts(i)
        // e.g. 32B = 8W, 6的时候仅0(6), 1(7)有效
        if (i == 0) fetchWord.valid := True
        else fetchWord.valid := pcWordOffset < icache.lineWords - i
        fetchWord.payload := Mux(hit, hitData(i), storedPacket(i))
      }
    }

    // 解决应该清空cache的条件（只需要设置valid）
    val commit = new Area {
      val cacheCommit = pipeline.globalService(classOf[CommitPlugin])
      val operation = cacheCommit.cacheOp

      val cacheOp = operation.op
      val addr = operation.addr
      val targetICache = cacheCommit.cacheOp.payload.sel === CacheSelType.ICache

      val way = addr(0, log2Up(icache.ways) bits)
      val index = addr(icache.indexRange)

      when(operation.valid && targetICache) {
        switch(cacheOp) {
          is(CacheOpType.None) {
            // do nothing
          }
          is(CacheOpType.IndexInvalidate) {
            valids(index)(way) := False
          }
          is(CacheOpType.HitInvalidate) {
            // lazy implementation invalidates all ways (VIPT)
            valids(index).foreach(_ := False)
          }
          is(CacheOpType.StoreTag) {
            for (i <- 0 until icache.sets) {
              for (j <- 0 until icache.ways) {
                valids(i)(j) := False
              }
            }
          }
        }
      }
    }
  }
}
