package NOP.pipeline.priviledge

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.LoongArch
import NOP.pipeline.core._
import NOP.pipeline.fetch._
import NOP.pipeline._

class InterruptHandlerPlugin(config: MyCPUConfig) extends Plugin[MyCPUCore] {
  var CSRMan: CSRPlugin = null
  var excHandler: ExceptionHandlerPlugin = null

  // ! Registers
  val TCFG_EN = RegInit(False)
  val TCFG_PERIODIC = RegInit(False)
  val TCFG_INITVAL = RegInit(U(0x0, 30 bits))
  val timer_en = out(RegInit(False))

  // ! TID
  val TID_TID = RegInit(U(0x0, 32 bits))

  // ! TVAL
  val TVAL_TIMEVAL = out(RegInit(U(0x0, 32 bits)))

  val setTimerInterrupt = False // Set this to true will trigger a timer interrupt

  // ! Indicate if there is a pending interrupt
  val intPending = Bool()

  val ECFG = out(Bits(12 bits))
  val ESTAT = out(Bits(12 bits))

  override def setup(pipeline: MyCPUCore): Unit = {
    import LoongArch.CSRAddress
    CSRMan = pipeline.service(classOf[CSRPlugin])
    excHandler = pipeline.service(classOf[ExceptionHandlerPlugin])

    // ! Registering CSR
    // ! TID
    CSRMan.rw(CSRAddress.TID, 0 -> TID_TID)

    // ! TCFG
    CSRMan.rw(
      CSRAddress.TCFG,
      0 -> TCFG_EN,
      1 -> TCFG_PERIODIC,
      2 -> TCFG_INITVAL
    )
    CSRMan.w1(CSRAddress.TCFG, 0)({
      timer_en := True
    })

    // ! TVAL
    CSRMan.r(
      CSRAddress.TVAL,
      0 -> TVAL_TIMEVAL
    )

    // ! TICLR
    CSRMan.r0(CSRAddress.TICLR, 1, 31 bits)

  }

  override def build(pipeline: MyCPUCore): Unit = {
    import LoongArch.CSRAddress
    // Send timer interrupt
    excHandler.ESTAT_IS_11 := excHandler.ESTAT_IS_11 | setTimerInterrupt.asBits
    CSRMan.r0(CSRAddress.TICLR, 0, 1 bits)
    CSRMan.w1(CSRAddress.TICLR, 0)({
      excHandler.ESTAT_IS_11 := False.asBits
    })

    excHandler.ESTAT_IS_2 := pipeline.io.intrpt

    // Downsampling the clock
    val innerClk = Bool() // Use this to count inner clock
    val innerCounter = Counter(config.interrupt.innerCounterDownEvery)
    innerCounter.increment()
    innerClk := innerCounter.willOverflow

    // Timer Logic
    when(timer_en && TVAL_TIMEVAL === 0) {
      setTimerInterrupt.set()
      timer_en := TCFG_PERIODIC
    }

    when(timer_en) {
      when(innerClk && TVAL_TIMEVAL =/= 0) {
        TVAL_TIMEVAL := TVAL_TIMEVAL - 1
      }

      when(TVAL_TIMEVAL === 0) {
        when(TCFG_PERIODIC) {
          TVAL_TIMEVAL := TCFG_INITVAL @@ U(0x0, 2 bits)
        } otherwise {
          TVAL_TIMEVAL := U(0x0, 32 bits) - 1
        }
      }
    }

    CSRMan.wAny(CSRAddress.TCFG)(data => {
      when(data(0)) {
        TVAL_TIMEVAL := (data(31 downto 2) ## U(0x0, 2 bits)).asUInt
      }
    })

    // INT pending logic
    intPending := ((excHandler.ECFG_LIE_11 ## excHandler.ECFG_LIE_0) &
      (excHandler.ESTAT_IS_12 ## excHandler.ESTAT_IS_11 ## excHandler.ESTAT_IS_2 ## excHandler.ESTAT_IS_0)).orR & excHandler.CRMD_IE & !excHandler.LLBCTL_LLBIT
    ECFG := (excHandler.ECFG_LIE_11 ## excHandler.ECFG_LIE_0)
    ESTAT := (excHandler.ESTAT_IS_12 ## excHandler.ESTAT_IS_11 ## excHandler.ESTAT_IS_2 ## excHandler.ESTAT_IS_0)
  }
}
