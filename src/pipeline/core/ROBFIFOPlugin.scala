package NOP.pipeline.core

import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.`enum`.LoadStoreType
import NOP.pipeline._
import NOP.pipeline.decode._
import NOP.pipeline.core._

import scala.collection.mutable.ArrayBuffer

class ROBFIFOPlugin(config: MyCPUConfig) extends Plugin[MyCPUCore] {
  private val prfAddressWidth = config.regFile.prfAddrWidth
  private val robDepth = config.rob.robDepth
  private val retireWidth = config.rob.retireWidth
  private val decodeWidth = config.decode.decodeWidth
  private val addressWidth = config.rob.robAddressWidth
  private val entryType = HardType(ROBEntryBundle(config))
  private val infoType = HardType(ROBEntryInfoBundle(config))
  private val stateType = HardType(ROBEntryStateBundle(config.regFile))
  require(isPow2(robDepth))

  val robInfo = new MultiPortFIFOSyncImpl(infoType, robDepth, decodeWidth, retireWidth) {
    val flush = in(Bool)
    when(flush) {
      pushPtr := 0
      popPtr := 0
      isRisingOccupancy := False
      // 可pop不可push，否则数据会丢失
      // fifoIO.push.foreach(_.setBlocked())
    }
    pushPtr.asOutput()
    popPtr.asOutput()
    popCount.asOutput()
  }

  // ROB state随机写口
  private val completePorts = ArrayBuffer[Flow[UInt]]()
  private val lsuPorts = ArrayBuffer[Flow[ROBStateLSUPortBundle]]()
  private val bruPorts = ArrayBuffer[Flow[ROBStateBRUPortBundle]]()
  private val aluPorts = ArrayBuffer[Flow[ROBStateALUPortBundle]]()
  def completePort: Flow[UInt] = {
    val port = Flow(UInt(config.rob.robAddressWidth bits))
    completePorts += port
    port
  }
  def lsuPort = {
    val port = Flow(ROBStateLSUPortBundle(config))
    lsuPorts += port
    port
  }
  def bruPort = {
    val port = Flow(ROBStateBRUPortBundle(config))
    bruPorts += port
    port
  }
  def aluPort = {
    val port = Flow(ROBStateALUPortBundle(config))
    aluPorts += port
    port
  }

  val robState = Reg(Vec(stateType, robDepth))

  val pushPtr = robInfo.pushPtr
  val popPtr = robInfo.popPtr
  val fifoIO = new Bundle {

    /** push的valid，pop的ready，都要遵循连续性，从头开始的第一个0就表示了停止的位置，后面的1都会被忽略。
      */
    val push = Vec(Stream(ROBEntryBundle(config, false)), decodeWidth)
    val pop = Vec(Stream(entryType), retireWidth)
    // 同步清空FIFO
    val flush = Bool
  }

  // TODO: [NOP] delete this debugging code
  val debug_fifoIO = out(new Bundle {
    val push = Vec(Stream(ROBEntryBundle(config, false)), decodeWidth)
    val pop = Vec(Stream(entryType), retireWidth)
    val flush = Bool()
  })
  debug_fifoIO.assignAllByName(fifoIO)

  val debugPopWriteData = config.debug generate out(Vec(BWord(), retireWidth))
  override def build(pipeline: MyCPUCore): Unit = pipeline plug new Area {
    if (config.debug) {
      val physRegs = pipeline.service(classOf[PhysRegFilePlugin]).regs
      for (i <- 0 until retireWidth) {
        debugPopWriteData(i) := physRegs(fifoIO.pop(i).payload.info.rename.wReg) // bypassing for debug :)
      }
    }
    // FIFO与MultiPortFIFOVec完全一致，但是ROB不止FIFO端口
    // multi-port FIFO io
    for (i <- 0 until retireWidth)
      fifoIO.pop(i).translateFrom(robInfo.io.pop(i)) { (entry, info) =>
        entry.info := info
        entry.state.setAsReg().allowOverride
        entry.state := robState(popPtr + robInfo.popCount + i)
      }

    for (i <- 0 until decodeWidth) {
      robInfo.io.push(i).translateFrom(fifoIO.push(i)) { (info, entry) =>
        info := entry.info
      }
      when(fifoIO.push(i).fire) {
        robState(pushPtr + i).assignSomeByName(fifoIO.push(i).payload.state)
        for (j <- i until retireWidth) when(pushPtr + i === popPtr + robInfo.popCount + j) {
          fifoIO.pop(j).payload.state.assignSomeByName(fifoIO.push(i).payload.state)
        }
      }
    }
    robInfo.flush := fifoIO.flush

    // random write ports
    println("ROB port summary:")
    printf("  pure ALU: %d\n", aluPorts.size)
    printf("  ALU&BRU: %d\n", bruPorts.size)
    printf("  LSU: %d\n", lsuPorts.size)
    printf("  MDU: %d\n", completePorts.size)
    val portCount = aluPorts.size + bruPorts.size + lsuPorts.size + completePorts.size
    printf("  issue width: %d\n", portCount)

    val defaultState = new ROBEntryStateBundle(config.regFile, true)

    // 完成则代表这条指令已经可以提交
    defaultState.complete := False
    // 完整异常信息
    defaultState.except.setIdle()
    // 分支预测恢复信息
    defaultState.mispredict := False
    defaultState.actualTaken := False

    // When full is true
    // LSU检测到uncached区段，需要提交时操作
    defaultState.lsuUncached := False
    // INT执行结果
    defaultState.intResult := 0

    // TODO: [NOP] DiffTest Bundle. Remove this in the future.
    defaultState.isCount := False
    defaultState.count64ReadValue := 0
    defaultState.csrRstat := False
    defaultState.csrRdata := 0
    defaultState.isLoad := False
    defaultState.isStore := False
    defaultState.isLL := False
    defaultState.isSC := False
    defaultState.lsType := LoadStoreType.WORD
    defaultState.vAddr := 0
    defaultState.pAddr := 0
    defaultState.storeData := 0
    defaultState.myPC := B(0x0eadbeef, 32 bits).asUInt

    for (p <- aluPorts)
      when(p.valid) {
        robState(p.robIdx) := defaultState
        robState(p.robIdx).allowOverride()
        robState(p.robIdx).complete := True
        robState(p.robIdx).assignSomeByName(p.payload)
        for (j <- 0 until retireWidth) when(p.robIdx === popPtr + robInfo.popCount + j) {
          fifoIO.pop(j).payload.state := defaultState
          fifoIO.pop(j).payload.state.allowOverride()
          fifoIO.pop(j).payload.state.complete := True
          fifoIO.pop(j).payload.state.assignSomeByName(p.payload)
        }
      }
    for (p <- bruPorts)
      when(p.valid) {
        robState(p.robIdx) := defaultState
        robState(p.robIdx).allowOverride()
        robState(p.robIdx).complete := True
        robState(p.robIdx).assignSomeByName(p.payload)
        for (j <- 0 until retireWidth) when(p.robIdx === popPtr + robInfo.popCount + j) {
          fifoIO.pop(j).payload.state := defaultState
          fifoIO.pop(j).payload.state.allowOverride()
          fifoIO.pop(j).payload.state.complete := True
          fifoIO.pop(j).payload.state.assignSomeByName(p.payload)
        }
      }
    for (p <- lsuPorts)
      when(p.valid) {
        robState(p.robIdx) := defaultState
        robState(p.robIdx).allowOverride()
        robState(p.robIdx).complete := True
        robState(p.robIdx).assignSomeByName(p.payload)
        for (j <- 0 until retireWidth) when(p.robIdx === popPtr + robInfo.popCount + j) {
          fifoIO.pop(j).payload.state := defaultState
          fifoIO.pop(j).payload.state.allowOverride()
          fifoIO.pop(j).payload.state.complete := True
          fifoIO.pop(j).payload.state.assignSomeByName(p.payload)
        }
      }
    for (p <- completePorts)
      when(p.valid) {
        robState(p.payload) := defaultState
        robState(p.payload).allowOverride()
        robState(p.payload).complete := True
        for (j <- 0 until retireWidth) when(p.payload === popPtr + robInfo.popCount + j) {
          fifoIO.pop(j).payload.state := defaultState
          fifoIO.pop(j).payload.state.allowOverride()
          fifoIO.pop(j).payload.state.complete := True
        }
      }
  }
}
