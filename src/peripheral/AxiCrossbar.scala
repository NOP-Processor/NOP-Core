package NOP.peripheral

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._

class AxiCrossbar(axiConfig: Axi4Config) extends Component {
  val io = new Bundle {
    val iBus, dBus, udBus = slave(Axi4(axiConfig)).setBlocked()
    val cpuBus = master(Axi4(axiConfig)).setIdle()
  }

  val writeArb = new Area {
    val busList = Seq(io.dBus, io.udBus)
    val busWithIndex = busList.zipWithIndex
    val busy = RegInit(False)
    val grantId = Reg(UInt(1 bits))
    val hasReq = busList.map(_.aw.valid).orR
    when(!busy && hasReq) {
      busy := True
      for ((bus, i) <- busWithIndex) when(bus.aw.valid) { grantId := i }
    }
    when(busy) {
      for ((bus, i) <- busWithIndex) when(grantId === i) {
        bus.aw >> io.cpuBus.aw
        bus.w >> io.cpuBus.w
        bus.b << io.cpuBus.b
      }
    }
    busy.clearWhen(io.cpuBus.b.fire)
  }

  val readArb = new Area {
    val busList = Seq(io.iBus, io.dBus, io.udBus)
    val busWithIndex = busList.zipWithIndex
    val busy = RegInit(False)
    val grantId = Reg(UInt(2 bits))
    val hasReq = busList.map(_.ar.valid).orR
    when(!busy && hasReq) {
      busy := True
      for ((bus, i) <- busWithIndex) when(bus.ar.valid) { grantId := i }
    }
    when(busy) {
      for ((bus, i) <- busWithIndex) when(grantId === i) {
        bus.ar >> io.cpuBus.ar
        bus.r << io.cpuBus.r
      }
    }
    busy.clearWhen(io.cpuBus.r.fire && io.cpuBus.r.last)
  }
}
