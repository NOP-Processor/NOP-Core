package NOP.pipeline

import NOP.builder._
import NOP.pipeline._
import NOP._

import spinal.core._
import spinal.lib._

trait ExecutePipeline extends Pipeline {
  type T = ExecutePipeline
  val ISS: Stage = null // issue/select
  val RRD: Stage = null // register read
  val EXE: Stage = null
  val WB: Stage = null
}
