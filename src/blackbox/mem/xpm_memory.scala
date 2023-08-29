package NOP.blackbox.mem

import spinal.core._

class xpm_memory_sdpram_generic extends Generic {
  var ADDR_WIDTH_A = 6
  var ADDR_WIDTH_B = 6
  var AUTO_SLEEP_TIME = 0
  var BYTE_WRITE_WIDTH_A = 32
  var CASCADE_HEIGHT = 0
  var CLOCKING_MODE = "common_clock"
  var ECC_MODE = "no_ecc"
  var MEMORY_INIT_FILE = "none"
  var MEMORY_INIT_PARAM = "0"
  var MEMORY_OPTIMIZATION = "true"
  var MEMORY_PRIMITIVE = "auto"
  var MEMORY_SIZE = 2048
  var MESSAGE_CONTROL = 0
  var READ_DATA_WIDTH_B = 32
  var READ_LATENCY_B = 2
  var READ_RESET_VALUE_B = "0"
  var RST_MODE_A = "SYNC"
  var RST_MODE_B = "SYNC"
  var SIM_ASSERT_CHK = 0
  var USE_EMBEDDED_CONSTRAINT = 0
  var USE_MEM_INIT = 0
  var WAKEUP_TIME = "disable_sleep"
  var WRITE_DATA_WIDTH_A = 32
  var WRITE_MODE_B = "no_change"
}

class xpm_memory_sdpram(param: xpm_memory_sdpram_generic) extends BlackBox {
  val generic = param

  val io = new Bundle {
    val read = new Bundle {
      val clk = in(Bool)
      val rst = in(Bool)
      val en = in(Bool)
      val regce = in(Bool)
      val addr = in(UInt(generic.ADDR_WIDTH_B bits))
      val dout = out(Bits(generic.READ_DATA_WIDTH_B bits))
      val sbiterr = out(Bool)
      val dbiterr = out(Bool)
    }
    val write = new Bundle {
      val clk = in(Bool)
      val en = in(Bool)
      val we = in(Bits(generic.WRITE_DATA_WIDTH_A / generic.BYTE_WRITE_WIDTH_A bits))
      val addr = in(UInt(generic.ADDR_WIDTH_A bits))
      val din = in(Bits(generic.WRITE_DATA_WIDTH_A bits))
      val injectsbiterr = in(Bool) default False
      val injectdbiterr = in(Bool) default False
    }
    val sleep = in(Bool) default False
  }

  addPrePopTask(() => {
    io.flatten.foreach(bt => {
      if (bt.getName().contains("write"))
        bt.setName(bt.getName().replace("write_", "") + "a")
      if (bt.getName().contains("read"))
        bt.setName(bt.getName().replace("read_", "") + "b")
    })
  })

  noIoPrefix
  mapClockDomain(clock = io.write.clk)
  mapClockDomain(clock = io.read.clk, reset = io.read.rst, resetActiveLevel = HIGH)
}
