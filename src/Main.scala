package NOP

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib._

import NOP._

object Main {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      targetDirectory = "./build",
      headerWithDate = true
    ).generateVerilog(new MyCPU(new MyCPUConfig()))
  }
}
