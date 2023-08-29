package NOP.pipeline.mem

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi.Axi4

import NOP._
import NOP.builder._
import NOP.utils._
import NOP.pipeline._
import NOP.pipeline.core._
import NOP.constants.enum._

class UncachedAccessPlugin(config: MyCPUConfig) extends Plugin[MemPipeline] {

  val udBus = Axi4(config.axiConfig).setIdle()
  udBus.b.ready.allowOverride := True

  val uncachedStoreHandshake = Event.setIdle()

  override def build(pipeline: MemPipeline): Unit = pipeline.MEM2 plug new Area {
    import pipeline.MEM2._
    import pipeline.signals._
    val std = input(STD_SLOT)
    val isLDU = std.valid && !std.isStore
    val isSTU = std.valid && std.isStore && !std.isCached

    when(isSTU) {
      // STU直接向FSM提交写memory，要等到脏行写完
      uncachedStoreHandshake.valid := arbitration.notStuck
      arbitration.haltItself.setWhen(!uncachedStoreHandshake.ready)
    }

    val uncachedStoreFSM = new StateMachine {
      disableAutoStart()
      setEntry(stateBoot)
      val waitAXIWriteU = new State

      // Register that samples std.payload
      // When starting another uncached store
      val regSTD = RegNextWhen(std.payload, uncachedStoreHandshake.fire)
      uncachedStoreHandshake.setBlocked() // ready := False

      val awSent, wSent = RegInit(False)
      val bRecv = RegInit(True)
      awSent setWhen (udBus.aw.fire)
      wSent setWhen (udBus.w.fire)
      bRecv setWhen (udBus.b.fire)

      stateBoot.whenIsActive {
        uncachedStoreHandshake.ready := (bRecv || udBus.b.fire)
        when(uncachedStoreHandshake.fire) {
          awSent := False
          wSent := False
          bRecv := False
          goto(waitAXIWriteU)
        }
      }
      // uncached store
      waitAXIWriteU.whenIsActive {
        val aw = udBus.aw
        aw.valid := !awSent
        aw.payload.id := 2
        aw.payload.addr := regSTD.addr
        aw.payload.len := 0
        aw.payload.size := LoadStoreType.toAxiSize(regSTD.lsType)
        aw.payload.burst := B(1, 2 bits)
        aw.payload.lock := 0
        aw.payload.cache := 0
        if (config.axiConfig.useQos) aw.payload.qos := 0
        aw.payload.prot := 0
        val w = udBus.w
        w.valid := !wSent
        w.data := regSTD.data
        w.strb := regSTD.be
        w.last := True
        when((awSent || aw.fire) && (wSent || w.fire)) { goto(stateBoot) }
      }
    }

  }
}
