package NOP.pipeline.decode

import spinal.core._
import spinal.lib._

import NOP._
import NOP.builder._
import NOP.pipeline._

final case class RegRenameBundle(arfAddrWidth: Int, prfAddrWidth: Int) extends Bundle with IMasterSlave {
  val req = Flow(UInt(arfAddrWidth bits))
  val rsp = UInt(prfAddrWidth bits)
  override def asMaster(): Unit = {
    master(req)
    in(rsp)
  }
}
