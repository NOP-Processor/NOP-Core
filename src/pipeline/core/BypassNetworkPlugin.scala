package NOP.pipeline.core

import spinal.core._
import spinal.lib._

import NOP.builder._
import scala.collection.mutable
import NOP._
import NOP.utils._
import NOP.constants.enum._
import NOP.pipeline.core._

class BypassNetworkPlugin(config: RegFileConfig) extends Plugin[MyCPUCore] {
  val writePorts = mutable.ArrayBuffer[Flow[RegWriteBundle]]()
  private val readPorts = mutable.ArrayBuffer[(UInt, Flow[Bits])]()

  def writePort = {
    val port = Flow(RegWriteBundle(config.prfAddrWidth))
    writePorts += port
    port
  }

  def readPort(addr: UInt) = {
    val port = Flow(BWord()).setIdle()
    readPorts += (addr -> port)
    port
  }

  override def build(pipeline: MyCPUCore): Unit = {
    // 前传逻辑
    readPorts.foreach { r =>
      // r._1 是地址，r._2 是数据
      // p0 永远不应该被写，它是 r0 的固定映射
      // 永远不应该出现写冲突，rename处理掉了写冲突
      val writeOH = writePorts.map { p => p.valid && p.payload.addr === r._1 }
      when(writeOH.orR) { r._2.push(MuxOH(writeOH, writePorts.map(_.payload.data))) }
    }
  }
}
