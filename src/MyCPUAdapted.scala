package NOP

import NOP.constants.LoongArch.CSRAddress
import NOP.constants.`enum`.{CacheOpType, CacheSelType, LoadStoreType, TLBOpType}
import spinal.core.{Bits, _}
import spinal.lib.bus.amba4.axi._
import spinal.lib._
import NOP.debug._
import NOP.pipeline.core._
import NOP.pipeline.priviledge._
import NOP.utils._

class MyCPUAdapted(config: MyCPUConfig) extends Component {
  setDefinitionName("mycpu_top")
  noIoPrefix()

  val io = new Bundle {
    val aclk = in(Bool)
    val aresetn = in(Bool)
    // Interrupts
    val intrpt = in(Bits(8 bits))

    val iBus_ = master(Axi4(config.axiConfig))
    val dBus_ = master(Axi4(config.axiConfig))
    val udBus_ = master(Axi4(config.axiConfig))
  }

  val defaultClockDomain = ClockDomain(
    clock = io.aclk,
    reset = io.aresetn,
    config = ClockDomainConfig(resetActiveLevel = LOW)
  )

  val defaultClockArea = new ClockingArea(defaultClockDomain) {
    val cpu = new NOP.pipeline.core.MyCPUCore(config)

    // Connect io to CPUCore
    cpu.io.intrpt := io.intrpt

    cpu.io.iBus <> io.iBus_
    cpu.io.dBus <> io.dBus_
    cpu.io.udBus <> io.udBus_

  }.setCompositeName(this)

  addPrePopTask { () =>
    Axi4Rename.Rename(io) // To make Axi4 signals match with loongchip
  }

}
