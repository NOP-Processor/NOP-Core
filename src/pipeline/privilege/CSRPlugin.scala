package NOP.pipeline.priviledge

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import NOP._
import NOP.utils._
import NOP.builder._
import NOP.constants.LoongArch.CSRAddress
import NOP.pipeline.core._
import NOP.pipeline.fetch._
import NOP.pipeline._

trait CsrDataField {
  val that: Data
  val bitOffset: Int
}
case class CsrWrite(that: Data, bitOffset: Int) extends CsrDataField
case class CsrRead(that: Data, bitOffset: Int) extends CsrDataField
case class CsrOnWrite(bitOffset: Int, doThat: Bits => Unit)

final case class CSRWriteBundle() extends Bundle {
  val addr: UInt = UInt(14 bits)
  val data: Bits = Bits(32 bits)
}

class CSRPlugin extends Plugin[MyCPUCore] {

  // TODO: [NOP] Remove these signals
  val DifftestCSRRegStateCRMD = out(Bits(64 bits))
  val DifftestCSRRegStatePRMD = out(Bits(64 bits))
  val DifftestCSRRegStateEUEN = out(Bits(64 bits))
  val DifftestCSRRegStateECFG = out(Bits(64 bits))
  val DifftestCSRRegStateESTAT = out(Bits(64 bits))
  val DifftestCSRRegStateERA = out(Bits(64 bits))
  val DifftestCSRRegStateBADV = out(Bits(64 bits))
  val DifftestCSRRegStateEENTRY = out(Bits(64 bits))
  val DifftestCSRRegStateTLBIDX = out(Bits(64 bits))
  val DifftestCSRRegStateTLBEHI = out(Bits(64 bits))
  val DifftestCSRRegStateTLBELO0 = out(Bits(64 bits))
  val DifftestCSRRegStateTLBELO1 = out(Bits(64 bits))
  val DifftestCSRRegStateASID = out(Bits(64 bits))
  val DifftestCSRRegStatePGDL = out(Bits(64 bits))
  val DifftestCSRRegStatePGDH = out(Bits(64 bits))
  val DifftestCSRRegStateSAVE0 = out(Bits(64 bits))
  val DifftestCSRRegStateSAVE1 = out(Bits(64 bits))
  val DifftestCSRRegStateSAVE2 = out(Bits(64 bits))
  val DifftestCSRRegStateSAVE3 = out(Bits(64 bits))
  val DifftestCSRRegStateTID = out(Bits(64 bits))
  val DifftestCSRRegStateTCFG = out(Bits(64 bits))
  val DifftestCSRRegStateTVAL = out(Bits(64 bits))
  val DifftestCSRRegStateTICLR = out(Bits(64 bits))
  val DifftestCSRRegStateLLBCTL = out(Bits(64 bits))
  val DifftestCSRRegStateTLBRENTRY = out(Bits(64 bits))
  val DifftestCSRRegStateDMW0 = out(Bits(64 bits))
  val DifftestCSRRegStateDMW1 = out(Bits(64 bits))

  // Connected to CommitPlugin
//  val askRead: Bool = False  // Not used
  val askWrite: Bool = Bool()
  val writeAddress: UInt = UInt(14 bits)
  val writeData: Bits = BWord()

  // Read Port
  val readAddress: UInt = UInt(14 bits)
  val readDataInit = BWord(0)
  val readData = CombInit(readDataInit)

  val doWrite: Bool = CombInit(askWrite)
//  val doRead: Bool = CombInit(askRead)

  // Interface for CSR
  private val mapping = mutable.LinkedHashMap[Int, mutable.ArrayBuffer[Any]]()

  protected def addMappingAt(address: Int, that: Any) =
    mapping.getOrElseUpdate(address, new mutable.ArrayBuffer[Any]) += that

  // data field
  def r(csrAddress: Int, bitOffset: Int, that: Data): Unit = addMappingAt(csrAddress, CsrRead(that, bitOffset))
  def w(csrAddress: Int, bitOffset: Int, that: Data): Unit = addMappingAt(csrAddress, CsrWrite(that, bitOffset))
  def rw(csrAddress: Int, bitOffset: Int, that: Data): Unit = {
    r(csrAddress, bitOffset, that)
    w(csrAddress, bitOffset, that)
  }
  def r0(csrAddress: Int, bitOffset: Int, bitWidth: BitCount): Unit =
    addMappingAt(csrAddress, CsrRead(U(0, bitWidth.value bits), bitOffset))
  def w1(csrAddress: Int, bitOffset: Int)(body: => Unit): Unit = {
    addMappingAt(
      csrAddress,
      CsrOnWrite(
        bitOffset,
        data => {
          when(data(bitOffset)) { body }
        }
      )
    )
  }
  def wAny(csrAddress: Int)(body: Bits => Unit): Unit = {
    addMappingAt(csrAddress, CsrOnWrite(0, body))
  }

  def rw(csrAddress: Int, thats: (Int, Data)*): Unit = for (that <- thats)
    rw(csrAddress, that._1, that._2)
  def w(csrAddress: Int, thats: (Int, Data)*): Unit = for (that <- thats)
    w(csrAddress, that._1, that._2)
  def r(csrAddress: Int, thats: (Int, Data)*): Unit = for (that <- thats)
    r(csrAddress, that._1, that._2)

  def rw[T <: Data](csrAddress: Int, that: T): Unit = rw(csrAddress, 0, that)
  def w[T <: Data](csrAddress: Int, that: T): Unit = w(csrAddress, 0, that)
  def r[T <: Data](csrAddress: Int, that: T): Unit = r(csrAddress, 0, that)

  def rCsr(csrAddress: Int, that: Bits): Unit = {
    val newWire = out(B(0x0, that.getBitsWidth bits))
    for ((address, jobs) <- mapping) {
      if (address == csrAddress) {
        for (element <- jobs) element match {
          case element: CsrRead if element.that.getBitsWidth != 0 =>
            newWire(
              element.bitOffset,
              element.that.getBitsWidth bits
            ) := element.that.asBits
          case _ =>
        }
      }
    }
    that := newWire
  }

  // logic generation
  private def doReadJobs(jobs: mutable.ArrayBuffer[Any]): Unit = {
    for (element <- jobs) element match {
//      case element: CsrOnRead => when(doRead)(element.doThat())
      case element: CsrRead if element.that.getBitsWidth != 0 =>
        readDataInit(
          element.bitOffset,
          element.that.getBitsWidth bits
        ) := element.that.asBits
      case _ =>
    }
  }

  private def doWriteJobs(jobs: mutable.ArrayBuffer[Any]): Unit = {
    for (element <- jobs) element match {
      case element: CsrWrite =>
        when(doWrite)(
          element.that.assignFromBits(
            writeData(
              element.bitOffset,
              element.that.getBitsWidth bits
            )
          )
        )
      case element: CsrOnWrite =>
        when(doWrite)(
          element.doThat(
            writeData
          )
        )
      case _ =>
    }
  }

  protected def genRead() = switch(readAddress) {
    for ((address, jobs) <- mapping) {
      is(address) {
        doReadJobs(jobs)
      }
    }
  }

  protected def genWrite() = switch(writeAddress) {
    for ((address, jobs) <- mapping) {
      is(address) {
        doWriteJobs(jobs)
      }
    }
  }

  override def build(pipeline: MyCPUCore): Unit = pipeline plug new Area {
    // write logic
    val csrWritePort = pipeline.service(classOf[CommitPlugin]).CSRWrite
    writeAddress := csrWritePort.payload.addr
    writeData := csrWritePort.payload.data
    askWrite := csrWritePort.valid

    rCsr(CSRAddress.CRMD, DifftestCSRRegStateCRMD)
    rCsr(CSRAddress.PRMD, DifftestCSRRegStatePRMD)
    rCsr(CSRAddress.EUEN, DifftestCSRRegStateEUEN)
    rCsr(CSRAddress.ECFG, DifftestCSRRegStateECFG)
    rCsr(CSRAddress.ESTAT, DifftestCSRRegStateESTAT)
    rCsr(CSRAddress.ERA, DifftestCSRRegStateERA)
    rCsr(CSRAddress.BADV, DifftestCSRRegStateBADV)
    rCsr(CSRAddress.EENTRY, DifftestCSRRegStateEENTRY)
    rCsr(CSRAddress.TLBIDX, DifftestCSRRegStateTLBIDX)
    rCsr(CSRAddress.TLBEHI, DifftestCSRRegStateTLBEHI)
    rCsr(CSRAddress.TLBELO0, DifftestCSRRegStateTLBELO0)
    rCsr(CSRAddress.TLBELO1, DifftestCSRRegStateTLBELO1)
    rCsr(CSRAddress.ASID, DifftestCSRRegStateASID)
    rCsr(CSRAddress.PGDL, DifftestCSRRegStatePGDL)
    rCsr(CSRAddress.PGDH, DifftestCSRRegStatePGDH)
    rCsr(CSRAddress.SAVE0, DifftestCSRRegStateSAVE0)
    rCsr(CSRAddress.SAVE1, DifftestCSRRegStateSAVE1)
    rCsr(CSRAddress.SAVE2, DifftestCSRRegStateSAVE2)
    rCsr(CSRAddress.SAVE3, DifftestCSRRegStateSAVE3)
    rCsr(CSRAddress.TID, DifftestCSRRegStateTID)
    rCsr(CSRAddress.TCFG, DifftestCSRRegStateTCFG)
    rCsr(CSRAddress.TVAL, DifftestCSRRegStateTVAL)
    rCsr(CSRAddress.TICLR, DifftestCSRRegStateTICLR)
    rCsr(CSRAddress.LLBCTL, DifftestCSRRegStateLLBCTL)
    rCsr(CSRAddress.TLBRENTRY, DifftestCSRRegStateTLBRENTRY)
    rCsr(CSRAddress.DMW0, DifftestCSRRegStateDMW0)
    rCsr(CSRAddress.DMW1, DifftestCSRRegStateDMW1)

    // Translation of the csr mapping into real logic
    Component.current.afterElaboration {
      genRead()
      genWrite()
    }
  }
}
