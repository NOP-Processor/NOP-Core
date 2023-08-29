package NOP.peripheral

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._
import spinal.lib.fsm._

import NOP.blackbox.mem._

case class BufferEntry(config: Axi4Config) extends Bundle {
  val id = UInt(config.idWidth bits)
  val addr = UInt(config.addressWidth bits)
  val data = Bits(config.dataWidth bits)
  val size = UInt(3 bits)
  val strb = config.useStrb generate Bits(config.dataWidth / 8 bits)
}

class AxiBuffer(axiConfig: Axi4Config) extends Component {
  val io = new Bundle {
    val in_axi = slave(Axi4(axiConfig))
    val out_axi = master(Axi4(axiConfig))
  }
  val bufferType = HardType(BufferEntry(axiConfig))
  val dataWidth = bufferType.getBitsWidth

  val generic = new xpm_fifo_sync_generic()
  generic.READ_MODE = "std"
  generic.FIFO_WRITE_DEPTH = 1024
  generic.WRITE_DATA_WIDTH = dataWidth
  generic.READ_DATA_WIDTH = dataWidth

  val fifo = new xpm_fifo_sync(generic)
  fifo.io.rd_en := False
  fifo.io.wr_en := False
  fifo.io.din := B(0, dataWidth bits)

  io.in_axi.setBlocked()
  io.out_axi.setIdle()
  io.out_axi.b.ready.allowOverride := True
  io.in_axi.b.payload.resp.allowOverride := B(0, 2 bits) // OKAY

  val outaw = io.out_axi.aw
  val outw = io.out_axi.w
  val outb = io.out_axi.b
  val outar = io.out_axi.ar
  val outr = io.out_axi.r

  val inaw = io.in_axi.aw
  val inw = io.in_axi.w
  val inb = io.in_axi.b
  val inar = io.in_axi.ar
  val inr = io.in_axi.r

  val writeIsRunning = False

  val readOutputFSM = new StateMachine {
    disableAutoStart()
    setEntry(state = stateBoot)

    val waitR = new State

    stateBoot.whenIsActive {
      when(fifo.io.empty && inar.valid && !writeIsRunning) {
        inar <> outar
        inr <> outr
        goto(waitR)
      }
    }

    waitR.whenIsActive {
      inar <> outar
      inr <> outr
      when(inr.payload.last && inr.valid && inr.ready) {
        goto(stateBoot)
      }
    }
  }

  when(fifo.io.wr_rst_busy || fifo.io.rd_rst_busy || fifo.io.rst) {
    // Do nothing when rst is busy
  } elsewhen (!fifo.io.full && inaw.valid && !fifo.io.rd_en) {
    val entry = bufferType()

    fifo.io.wr_en := True
    fifo.io.din := entry.asBits

    inaw.ready := True
    inw.ready := True
    inb.valid := True
    // NOTE: !!!
    // udBus characteristics
    // 1. aw and w are valid at the same time
    // 2. len = 0
    entry.addr := inaw.payload.addr
    entry.size := inaw.payload.size
    entry.id := inaw.payload.id
    entry.data := inw.payload.data
    if (axiConfig.useStrb)
      entry.strb := inw.payload.strb
  }

  val writeOutputFSM = new StateMachine {
    disableAutoStart()
    setEntry(state = stateBoot)

    val waitAW = new State
    val waitRead = new State
    val waitW = new State
    val waitB = new State

    val regEntry = Reg(bufferType)

    stateBoot.whenIsActive {
      when(fifo.io.rd_rst_busy) {
        // Do nothing when rst is busy
      } otherwise {
        when(!fifo.io.empty) {
          writeIsRunning := True
          fifo.io.rd_en := True
          goto(waitRead)
        }
      }
    }

    waitRead.whenIsActive {
      writeIsRunning := True

      val entry = bufferType()
      entry.assignFromBits(fifo.io.dout)
      regEntry := entry

      outaw.valid := True
      outaw.payload.id := entry.id
      outaw.payload.addr := entry.addr
      outaw.len := 0 // 1 transfer only
      outaw.size := entry.size
      outaw.burst := 1 // increment

      outw.valid := True
      outw.payload.data := entry.data
      outw.payload.last := True
      if (axiConfig.useStrb)
        outw.payload.strb := entry.strb
      goto(waitAW)
    }

    waitAW.whenIsActive {
      writeIsRunning := True
      outaw.valid := True
      outaw.payload.id := regEntry.id
      outaw.payload.addr := regEntry.addr
      outaw.len := 0 // 1 transfer only
      outaw.size := regEntry.size
      outaw.burst := 1 // increment

      outw.valid := True
      outw.payload.data := regEntry.data
      outw.payload.last := True
      if (axiConfig.useStrb)
        outw.payload.strb := regEntry.strb
      when(outaw.ready && outw.ready) {
        when(outb.valid) {
          goto(stateBoot);
        } otherwise {
          goto(waitB);
        }
      } elsewhen (outaw.ready) {
        goto(waitW);
      }
    }

    waitW.whenIsActive {
      writeIsRunning := True
      outw.valid := True
      outw.payload.data := regEntry.data
      outw.payload.last := True
      if (axiConfig.useStrb)
        outw.payload.strb := regEntry.strb
      when(outw.ready && outb.valid) {
        goto(stateBoot)
      } elsewhen (outw.ready) {
        goto(waitB)
      }
    }

    waitB.whenIsActive {
      writeIsRunning := True
      when(outb.valid) {
        goto(stateBoot)
      }
    }
  }
}
