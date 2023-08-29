package NOP.pipeline.core

import NOP._
import NOP.debug._
import NOP.builder._
import NOP.pipeline._
import NOP.pipeline.fetch._
import NOP.pipeline.decode._
import NOP.pipeline.exe._
import NOP.pipeline.mem._
import NOP.pipeline.priviledge._
import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._

class MyCPUCore(config: MyCPUConfig) extends Component with MultiPipeline {

  val global = this

  // ! Fetch Pipeline
  val fetchPipeline = new FetchPipeline {

    override val signals = new FetchSignals(config)
    override val IF1: Stage = newStage()
    override val IF2: Stage = newStage()

    plugins ++= List(
      new ProgramCounterPlugin(config.frontend),
      new FetchBufferPlugin(config),
      new ICachePlugin(config),
      new ExceptionMuxPlugin[FetchPipeline](stages.size - 1),
      new InstAddrTranslatePlugin(),
      new GlobalPredictorBTBPlugin(config.frontend),
      new ReturnAddressStackPlugin(config.frontend)
    ).filter(_ != null)

  }
  addPipeline(fetchPipeline)

  // ! Decode Pipeline
  val decodePipeline: DecodePipeline = new DecodePipeline {

    override val signals = new DecodeSignals(config)
    override val ID: Stage = newStage()
    override val RENAME: Stage = newStage()
    override val DISPATCH: Stage = newStage()

    plugins ++= List(
      new DecoderArray(config, fetchPipeline.service(classOf[FetchBufferPlugin]).popPorts),
      new RenamePlugin(config)
    ).filter(_ != null)
  }
  addPipeline(decodePipeline)

  // val memPipeline: MemPipeline = null

  // ! INT Pipeline
  var exePipelines = Vector[ExecutePipeline]()
  for (i <- (0 until config.intIssue.issueWidth).reverse) {
    val exePipeline = new ExecutePipeline {
      override val ISS: Stage = newStage().setName(s"INT${i}_ISS")
      override val RRD: Stage = newStage().setName(s"INT${i}_RRD")
      override val EXE: Stage = newStage().setName(s"INT${i}_EXE")
      override val WB: Stage = newStage().setName(s"INT${i}_WB")
      plugins += new IntExecutePlugin(config, i)
    }
    exePipelines = exePipelines :+ exePipeline
    addPipeline(exePipeline)
  }

  // ! Mul / Div Pipeline
  val mulDivPipeline = new ExecutePipeline {
    override val ISS = newStage().setName("MULDIV_ISS")
    override val RRD = newStage().setName("MULDIV_RRD")
    override val EXE = newStage().setName("MULDIV_EXE")
    override val WB = newStage().setName("MULDIV_WB")
    plugins += new MulDivExecutePlugin(config)
  }
  addPipeline(mulDivPipeline)

  // ! Mem Pipeline
  val memPipeline = new MemPipeline {
    override val signals = new MemSignals(config)
    override val ISS: Stage = newStage().setName("MEM_ISS")
    override val RRD: Stage = newStage().setName("MEM_RRD")
    override val MEMADDR: Stage = newStage().setName("MEM_ADDR")
    override val MEM1: Stage = newStage().setName("MEM_MEM1")
    override val MEM2: Stage = newStage().setName("MEM_MEM2")
    override val WB: Stage = newStage().setName("MEM_WB")
    override val WB2: Stage = newStage().setName("MEM_WB2")

    plugins ++= List(
      new AddressGenerationPlugin(config),
      new DCachePlugin(config),
      new UncachedAccessPlugin(config),
      new LoadPostprocessPlugin(),
      new StoreBufferPlugin(config),
      new MemExecutePlugin(config),
      new ExceptionMuxPlugin[MemPipeline](stages.size - 1)
    ).filter(_ != null)
  }
  addPipeline(memPipeline)

  // ! IO
  val io = new Bundle {
    val intrpt = in(Bits(8 bits)) default 0
    val debug = config.debug generate out(new DebugInterface())
    val iBus = master(fetchPipeline.service(classOf[ICachePlugin]).iBus.toAxi4())
    val dBus = master(memPipeline.service(classOf[DCachePlugin]).dBus)
    val udBus = master(memPipeline.service(classOf[UncachedAccessPlugin]).udBus)
  }

  io.debug.wb.pc := 0
  io.debug.wb.inst := 0
  io.debug.wb.rf.wen := 0
  io.debug.wb.rf.wnum := 0
  io.debug.wb.rf.wdata := 0

  // ! Global Plugins
  type T = MyCPUCore
  plugins ++= List(
    new PhysRegFilePlugin(config.regFile),
    new ROBFIFOPlugin(config),
    new CommitPlugin(config),
    new BypassNetworkPlugin(config.regFile),
    // Reservation stations
    new IntIssueQueuePlugin(config),
    new MulDivIssueQueuePlugin(config),
    new MemIssueQueuePlugin(config),
    // Memory Speculative Wakeup
    new SpeculativeWakeupHandler(),
    // Privilege
    new Timer64Plugin(),
    new ExceptionHandlerPlugin(),
    new InterruptHandlerPlugin(config),
    new CSRPlugin(),
    new MMUPlugin(config),
    new WaitHandlerPlugin()
  ).filter(_ != null)

  // ! Stages
  val IF1: Stage = fetchPipeline.IF1
  val IF2: Stage = fetchPipeline.IF2
  val ID: Stage = decodePipeline.ID
  val RENAME: Stage = decodePipeline.RENAME
  val DISPATCH: Stage = decodePipeline.DISPATCH
  val EXE: Stage = null
  val MEM1: Stage = memPipeline.MEM1
  val MEM2: Stage = memPipeline.MEM2
  val WB: Stage = null

}
