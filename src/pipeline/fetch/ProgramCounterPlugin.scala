package NOP.pipeline.fetch

import NOP.pipeline._
import NOP.builder._
import NOP.utils._
import NOP._

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

class ProgramCounterPlugin(config: FrontendConfig) extends Plugin[FetchPipeline] {
  case class JumpInfo(interface: Stream[UInt], priority: Int)
  private val jumpInfos = ArrayBuffer[JumpInfo]()
  private var predict: Flow[UInt] = null

  // * outside functions
  // add a new entry to the jumpInfo, the larger the higher priority
  def addJumpInterface(interface: Stream[UInt], priority: Int): Unit = {
    jumpInfos += JumpInfo(interface, priority)
  }

  def setPredict(source: Flow[UInt]): Unit = {
    predict = source
  }

  // * inner signals
  val nextPC = UWord()
  val backendJumpInterface = Stream(UWord()).setIdle()
  override def build(pipeline: FetchPipeline): Unit = pipeline.IF1 plug new Area {
    import pipeline.IF1._
    val jumpPipe = backendJumpInterface.s2mPipe()
    jumpPipe.ready := !arbitration.isStuck
    val cacheLineWords = config.icache.lineWords

    // ! declare regPC
    val regPC = RegNextWhen(nextPC, !arbitration.isStuck, init = UWord(config.pcInit))
    insert(pipeline.signals.PC) := regPC

    // ! Set nextPC
    // ! nextPC branch 1, increment
    val pcOffset = regPC(2, log2Up(cacheLineWords) bits)
    val fetchWidth = config.fetchWidth

    val defaultPC = regPC + (pcOffset.muxList(
      U(fetchWidth), // default: increment by fetchWidth
      ((cacheLineWords - fetchWidth + 1) until cacheLineWords).map { i =>
        (i, U(cacheLineWords - i)) // 一次fetch不允许跨行，因此最多顶到行尾. 顶到行尾的时候，从下一行开始
      }
    ) @@ U"2'b00")
    nextPC := defaultPC

    // ! nextPC branch 2, BTB prediction jump
    if (predict != null) {
      when(predict.valid)(nextPC := predict.payload)
    }

    // ! nextPC branch 3 (highest priority), backend jump
    when(jumpPipe.valid)(nextPC := jumpPipe.payload)
  }
}
