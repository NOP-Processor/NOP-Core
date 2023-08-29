package NOP.pipeline.fetch

import NOP.pipeline._
import NOP.builder._
import NOP.utils._
import NOP._
import NOP.pipeline.core.CommitPlugin
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class CorrelatingPredictor(config: FrontendConfig, fetchWidth: Int) extends Component {
  val counterType = HardType(UInt(config.bpu.counterWidth bits))
  val ghrType = HardType(UInt(config.bpu.historyWidth bits))
  val io = new Bundle {
    val read = new Bundle {
      val enable = in(Bool)
      val nextPC = in(UWord())
      val predCounters = out(Vec(counterType, fetchWidth))
      val predTaken = out(Bits(fetchWidth bits))
      val globalHistory = out(ghrType())
    }
    // speculative update or restore
    val updateGHR = in(Flow(ghrType))
    val write = in(Flow(new Bundle {
      val pc = UWord()
      val ghr = ghrType()
      val newCounter = counterType()
    }))
  }
  // global history register
  val ghr = Reg(ghrType) init 0
  val nextGHR = ghrType()
  // pattern history table
  val pht = new ReorderCacheRAM(
    counterType,
    config.bpu.phtSets,
    fetchWidth,
    false
  )

  val memInit = Seq.fill(pht.wordsPerBank)(1)
  pht.rams.foreach(_.memGeneric.MEMORY_INIT_PARAM = memInit.mkString(","))

  io.read.globalHistory := ghr
  when(io.updateGHR.valid) { ghr := io.updateGHR.payload }
  nextGHR := Mux(io.updateGHR.valid, io.updateGHR.payload, ghr)

  pht.io.read.cmd.valid := io.read.enable
  pht.io.read.cmd.payload := nextGHR @@ io.read.nextPC(config.bpu.phtPCRange)
  io.read.predCounters := pht.io.read.rsp
  for (i <- 0 until fetchWidth) io.read.predTaken(i) := io.read.predCounters(i).msb

  pht.io.write.valid := io.write.valid
  pht.io.write.mask := 1
  pht.io.write.address := io.write.ghr @@ io.write.pc(config.bpu.phtPCRange)
  pht.io.write.data.assignDontCare()
  pht.io.write.data(0) := io.write.newCounter
}

class GlobalPredictorBTBPlugin(config: FrontendConfig) extends Plugin[FetchPipeline] {
  val numEntries = config.btb.sets
  val fetchWidth = config.fetchWidth
  val entriesLg2 = config.btb.indexWidth
  val btbConfig = config.btb

  val valid = Vec(RegInit(False), numEntries)
  val btb = new ReorderCacheRAM(BranchTableEntry(config.btb), numEntries, fetchWidth, false)
  val predictor = new CorrelatingPredictor(config, fetchWidth)

  val predictInterface = Flow(UWord())
  val jumpTarget = Reg(UWord())

  override def setup(pipeline: FetchPipeline): Unit = {
    val programCounter = pipeline.service(classOf[ProgramCounterPlugin])
    programCounter.setPredict(predictInterface)
  }

  override def build(pipeline: FetchPipeline): Unit = {

    import pipeline.IF1
    pipeline plug new Area {
      val nextPC = pipeline.service(classOf[ProgramCounterPlugin]).nextPC
      val index = nextPC(btbConfig.indexRange)
      btb.io.read.cmd.valid := !IF1.arbitration.isStuck
      btb.io.read.cmd.payload := index
      predictor.io.read.enable := IF1.arbitration.notStuck
      predictor.io.read.nextPC := nextPC
    }

    IF1 plug new Area {
      import IF1._
      import pipeline.signals._
      val tag = input(PC)(btbConfig.tagRange)
      val index = input(PC)(btbConfig.indexRange)
      val fetchWay = input(PC)(config.icache.wordOffsetRange)

      val hitData = btb.io.read.rsp

      insert(PREDICT_JUMP_FLAG) := False
      insert(PREDICT_JUMP_PAYLOAD) := hitData(0).statusBundle
      insert(PREDICT_JUMP_WAY) := 0

      // default with no prediction
      insert(INSTRUCTION_MASK).setAll
      insert(BRANCH_MASK).clearAll

      for (i <- fetchWidth - 1 downto 0) { // priority logic
        insert(PRED_COUNTER)(i) := 0
        val way = fetchWay + i
        when(way >= fetchWay) { // confirm not out of bound
          val entry = hitData(i)
          val validFlag = valid(index + i) && entry.tag === tag
          when(validFlag) { // matched entry
            insert(PRED_COUNTER)(i) := predictor.io.read.predCounters(i)
            insert(BRANCH_MASK)(i) := True // set branch mask
            when(predictor.io.read.predTaken(i)) { // predict taken
              insert(PREDICT_JUMP_FLAG) := True
              insert(PREDICT_JUMP_PAYLOAD) := entry.statusBundle
              insert(PREDICT_JUMP_WAY) := i
              if (i != fetchWidth - 1) {
                insert(INSTRUCTION_MASK) := (1 << (i + 1)) - 1 // set instruction mask
                // for example, when i=1 is jump, 0,1 are common instructions
                // so mask should be 0...0011 = 2^(1+1) - 1
                // if i = fetchwidth - 1, then all instructions are legal
              }
            }
          }
        }
      }
      insert(GLOBAL_BRANCH_HISTORY) := predictor.io.read.globalHistory
    }

    import pipeline.IF2
    IF2 plug new Area {
      // predict logic
      import pipeline.signals._
      import IF2._

      val tag = input(PC)(btbConfig.tagRange)
      val index = input(PC)(btbConfig.indexRange)
      val fetchWay = input(PC)(config.icache.wordOffsetRange)

      import pipeline.signals._
      val jumpFlag = input(PREDICT_JUMP_FLAG)
      val jumpPayload = input(PREDICT_JUMP_PAYLOAD)
      val jumpWay = input(PREDICT_JUMP_WAY)

      val lastValidWay = U(fetchWidth - 1)
      for (i <- fetchWidth - 1 downto 1) {
        when(!input(FETCH_PACKET).insts(i).valid) { lastValidWay := i - 1 }
      }

      val payloadTarget = jumpPayload.target @@ U(0, 2 bits)
      insert(PREDICT_ADDR) := payloadTarget
      val rasPredict = pipeline.service(classOf[ReturnAddressStackPlugin]).rasPredict
      when(rasPredict.valid) { payloadTarget := rasPredict.payload }

      predictInterface.payload := payloadTarget
      predictInterface.valid := False

      insert(TAKEN_MASK).clearAll

      when(arbitration.isValid) {
        when(jumpFlag) {
          insert(TAKEN_MASK)(jumpWay) := True
          predictInterface.valid := True
          IF2.arbitration.flushNext := True
        }
      }

      val branchCount = CountOne(input(INSTRUCTION_MASK) & input(BRANCH_MASK))
      val shiftedGHR = input(GLOBAL_BRANCH_HISTORY) |<< branchCount
      predictor.io.updateGHR.setIdle()
      when(arbitration.isFiring) {
        predictor.io.updateGHR.push(shiftedGHR | input(PREDICT_JUMP_FLAG).asUInt.resized)
      }
      insert(PRIVATE_BRANCH_HISTORY).foreach(
        _ := input(GLOBAL_BRANCH_HISTORY)
      )
    }

    pipeline plug new Area {
      // commit area
      val bpuCommit = pipeline.globalService(classOf[CommitPlugin])
      val predUpdate = bpuCommit.predUpdate

      btb.io.write.setIdle()
      predictor.io.write.setIdle()

      when(predUpdate.valid) { // time to update
        val payload = predUpdate.payload
        val pred = payload.predInfo
        val recover = payload.predRecover
        val tag = payload.pc(btbConfig.tagRange)
        val index = payload.pc(btbConfig.indexRange)

        btb.io.write.payload.address := index
        btb.io.write.payload.mask := B"1".resized
        predictor.io.write.pc := payload.pc
        predictor.io.write.ghr := recover.ghr

        when(pred.predictBranch && !payload.branchLike) {
          // Case 1, modified instruction, not branch anymore, must be mispredicted
          valid(index) := False // invalidate the entry
        } elsewhen (!pred.predictBranch && payload.branchLike) {
          // Case 2, new entry, must be mispredicted
          val newEntry = BranchTableEntry(config.btb)
          newEntry.tag := tag
          newEntry.statusBundle.isCall := payload.isCall
          newEntry.statusBundle.isReturn := payload.isRet
          when(payload.isCall || payload.isRet) {
            predictor.io.write.newCounter := 3
          } otherwise {
            predictor.io.write.newCounter := Mux(payload.isTaken, U(2, 2 bits), U(1, 2 bits))
          }
          newEntry.statusBundle.target := payload.target(31 downto 2)
          btb.io.write.payload.data(0) := newEntry
          btb.io.write.valid := True
          predictor.io.write.valid := True
          valid(index) := True
        } elsewhen (pred.predictBranch && payload.branchLike) { // update the entry
          val newEntry = BranchTableEntry(config.btb)
          newEntry.tag := tag
          newEntry.statusBundle.target := payload.target(31 downto 2)
          newEntry.statusBundle.isCall := payload.isCall
          newEntry.statusBundle.isReturn := payload.isRet

          when(payload.isTaken && recover.predictCounter =/= 3) {
            predictor.io.write.newCounter := recover.predictCounter + 1
          } elsewhen (!payload.isTaken && recover.predictCounter =/= 0) {
            predictor.io.write.newCounter := recover.predictCounter - 1
          } otherwise {
            predictor.io.write.newCounter := recover.predictCounter
          }
          btb.io.write.payload.data(0) := newEntry
          btb.io.write.valid := True
          predictor.io.write.valid := True
        }

        when(payload.mispredict) {
          // restore GHR
          predictor.io.updateGHR.push((predUpdate.predRecover.ghr @@ payload.isTaken).resized)
        }
      }
    }
  }
}
