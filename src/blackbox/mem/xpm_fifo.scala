package NOP.blackbox.mem

import spinal.core._

class xpm_fifo_sync_generic extends Generic {
  var DOUT_RESET_VALUE = "0" // String
  var ECC_MODE = "no_ecc" // String
  var FIFO_MEMORY_TYPE = "auto" // String
  var FIFO_READ_LATENCY = 1 // DECIMAL
  var FIFO_WRITE_DEPTH = 2048 // DECIMAL
  var FULL_RESET_VALUE = 0 // DECIMAL
  var PROG_EMPTY_THRESH = 10 // DECIMAL
  var PROG_FULL_THRESH = 10 // DECIMAL
  var RD_DATA_COUNT_WIDTH = 1 // DECIMAL
  var READ_DATA_WIDTH = 32 // DECIMAL
  var READ_MODE = "std" // String
  var SIM_ASSERT_CHK = 0 // DECIMAL; 0=disable simulation messages, 1=enable simulation messages
  var USE_ADV_FEATURES = "0707" // String
  var WAKEUP_TIME = 0 // DECIMAL
  var WRITE_DATA_WIDTH = 32 // DECIMAL
  var WR_DATA_COUNT_WIDTH = 1 // DECIMAL
}

class xpm_fifo_sync(param: xpm_fifo_sync_generic) extends BlackBox {
  val generic = param
  noIoPrefix()
  val io = new Bundle {
    val wr_clk, rst = in(Bool)
    val almost_empty, almost_full = out(Bool)
    val empty, full = out(Bool)
    val prog_empty, prog_full = out(Bool)
    val underflow, overflow = out(Bool)
    val data_valid = out(Bool)
    val dout = out(Bits(param.READ_DATA_WIDTH bits))
    val injectsbiterr, injectdbiterr = in(Bool) default False
    val sleep = in(Bool) default False
    val sbiterr, dbiterr = out(Bool)
    val rd_rst_busy, wr_rst_busy = out(Bool)
    val rd_en, wr_en = in(Bool)
    val wr_ack = out(Bool)
    val din = in(Bits(param.WRITE_DATA_WIDTH bits))
    val rd_data_count = out(UInt(param.RD_DATA_COUNT_WIDTH bits))
    val wr_data_count = out(UInt(param.WR_DATA_COUNT_WIDTH bits))
  }
  mapClockDomain(clock = io.wr_clk, reset = io.rst, resetActiveLevel = HIGH)
}
