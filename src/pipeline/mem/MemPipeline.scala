package NOP.pipeline.mem

import NOP.pipeline.priviledge._
import NOP.constants.enum._
import NOP.builder._
import NOP.pipeline._
import NOP.utils._
import NOP._

import spinal.core._
import spinal.lib._

class MemSignals(config: MyCPUConfig) {
  private val dcache = config.dcache
  private val iqDepth = config.memIssue.depth
  private val rPorts = config.regFile.rPortsEachInst
  private val prfAddrWidth = config.regFile.prfAddrWidth

  object ISSUE_SLOT extends Stageable(MemIssueSlot(config))
  object REG_READ_RSP extends Stageable(Vec(BWord(), rPorts))
  object WRITE_REG extends Stageable(Flow(UInt(prfAddrWidth bits)))
  object READ_REGS extends Stageable(Vec(UInt(prfAddrWidth bits), 2))
  object ROB_IDX extends Stageable(UInt(config.rob.robAddressWidth bits))
  object STD_SLOT extends Stageable(Flow(StoreBufferSlot(config)))

  object MEMORY_ADDRESS extends Stageable(UWord())
  object MEMORY_ADDRESS_PHYSICAL extends Stageable(UWord())
  object MEMORY_BE extends Stageable(Bits(4 bits))
  object MEMORY_READ_DATA extends Stageable(BWord())
  object MEMORY_WRITE_DATA extends Stageable(BWord())
  object TMP_RESULT extends Stageable(BWord())

  object DIRECT_TRANSLATE_RESULT extends Stageable(Flow(TranslateResultBundle()))
  object TLB_TRANSLATE_RESULT extends Stageable(Flow(TranslateResultBundle()))
  object TRANSLATE_SAVED_CSR extends Stageable(TranslateCSRBundle())

  object ADDRESS_CACHED extends Stageable(Bool())
  object IS_TLB_REFILL extends Stageable(Bool())
  object LOAD_STORE_TYPE extends Stageable(LoadStoreType())
  object IS_LOAD extends Stageable(Bool())
  object IS_STORE extends Stageable(Bool())
}

trait MemPipeline extends Pipeline {
  type T = MemPipeline
  val ISS: Stage = null // issue/select
  val RRD: Stage = null // register read
  val MEMADDR: Stage = null
  val MEM1: Stage = null
  val MEM2: Stage = null
  val WB: Stage = null
  val WB2: Stage = null // 注意读cache需要预留两个周期的冲突，所以增加一个空的WB阶段，用来前传
  val signals: MemSignals
}
