package NOP.blackbox.mem

import spinal.core._
import spinal.lib._

object noChange extends ReadUnderWritePolicy {
  override def readUnderWriteString: String = "noChange"
}

final case class SDPRAMSyncIOBundle[T <: Data](
    wordType: HardType[T],
    wordCount: Int,
    maskWidth: Int = -1
) extends Bundle {
  val read = slave(MemReadPort(wordType(), log2Up(wordCount)))
  val write = in(Flow(MemWriteCmd(Mem(wordType, wordCount))))
  val writeMask = maskWidth > 0 generate in(Bits(maskWidth bits))
}

trait SDPRAMSyncIO[T <: Data] {
  val io: SDPRAMSyncIOBundle[T]
  val memGeneric: xpm_memory_sdpram_generic
}

class SDPRAM[T <: Data](
    wordType: HardType[T],
    wordCount: Int,
    outputReg: Boolean,
    policy: ReadUnderWritePolicy = readFirst,
    simMessage: Boolean = true,
    useByteEnable: Boolean = false
) extends Component
    with SDPRAMSyncIO[T] {
  val addressWidth = log2Up(wordCount)
  val wordWidth = wordType.getBitsWidth
  val maskWidth = if (useByteEnable) wordWidth / 8 else -1
  if (useByteEnable) require(wordWidth % 8 == 0)
  val memGeneric = new xpm_memory_sdpram_generic
  memGeneric.ADDR_WIDTH_A = addressWidth
  memGeneric.ADDR_WIDTH_B = addressWidth
  memGeneric.BYTE_WRITE_WIDTH_A = if (useByteEnable) 8 else wordWidth
  memGeneric.MEMORY_SIZE = wordWidth * wordCount
  memGeneric.READ_DATA_WIDTH_B = wordWidth
  memGeneric.READ_LATENCY_B = if (outputReg) 2 else 1
  memGeneric.SIM_ASSERT_CHK = simMessage.toInt
  memGeneric.WRITE_DATA_WIDTH_A = wordWidth
  memGeneric.WRITE_MODE_B = policy.readUnderWriteString.replaceAll("([A-Z])", "_$1").toLowerCase()
  val mem = new xpm_memory_sdpram(memGeneric)
  val io = SDPRAMSyncIOBundle(wordType, wordCount, maskWidth)
  mem.io.read.en := io.read.cmd.valid
  mem.io.read.regce := io.read.cmd.valid
  mem.io.read.addr := io.read.cmd.payload
  io.read.rsp.assignFromBits(mem.io.read.dout)

  mem.io.write.en := io.write.valid
  mem.io.write.we := (if (useByteEnable) io.writeMask else mem.io.write.we.getAllTrue)
  mem.io.write.addr := io.write.payload.address
  mem.io.write.din := io.write.payload.data.asBits
}

class SDPRAMAsync[T <: Data](
    wordType: HardType[T],
    wordCount: Int,
    policy: ReadUnderWritePolicy = readFirst,
    simMessage: Boolean = true
) extends Component {
  val addressWidth = log2Up(wordCount)
  val wordWidth = wordType.getBitsWidth

  val memGeneric = new xpm_memory_sdpram_generic
  memGeneric.ADDR_WIDTH_A = addressWidth
  memGeneric.ADDR_WIDTH_B = addressWidth
  memGeneric.BYTE_WRITE_WIDTH_A = wordWidth // WE为1位，暂不使用
  memGeneric.MEMORY_SIZE = wordWidth * wordCount
  memGeneric.READ_DATA_WIDTH_B = wordWidth
  memGeneric.READ_LATENCY_B = 0
  memGeneric.SIM_ASSERT_CHK = simMessage.toInt
  memGeneric.WRITE_DATA_WIDTH_A = wordWidth
  memGeneric.WRITE_MODE_B = policy.readUnderWriteString.replaceAll("([A-Z])", "_$1").toLowerCase()
  val mem = new xpm_memory_sdpram(memGeneric)
  val io = new Bundle {
    val read = slave(MemReadPortAsync(wordType(), addressWidth))
    val write = in(Flow(MemWriteCmd(Mem(wordType, wordCount))))
  }
  mem.io.read.en := True
  mem.io.read.regce := True
  mem.io.read.addr := io.read.address
  io.read.data.assignFromBits(mem.io.read.dout)

  mem.io.write.en := io.write.valid
  mem.io.write.we := mem.io.write.we.getAllTrue // 恒1
  mem.io.write.addr := io.write.payload.address
  mem.io.write.din := io.write.payload.data.asBits
}

class SDPRAMWriteFirst[T <: Data](
    wordType: HardType[T],
    wordCount: Int
) extends SDPRAM(wordType, wordCount, false) {
  val regWriteData = RegNext(io.write.data)
  val regConflict = RegNext(io.write.valid && io.write.address === io.read.cmd.payload)
  when(regConflict)(io.read.rsp := regWriteData)
}

class SDPRAMWriteFirst2[T <: Data](
    wordType: HardType[T],
    wordCount: Int
) extends SDPRAM(wordType, wordCount, false, policy = writeFirst)

class SDPRAMAsyncToSyncWriteFirst[T <: Data](
    wordType: HardType[T],
    wordCount: Int,
    outputReg: Boolean = true,
    policy: ReadUnderWritePolicy = readFirst,
    simMessage: Boolean = true
) extends Component
    with SDPRAMSyncIO[T] {
  val addressWidth = log2Up(wordCount)
  val wordWidth = wordType.getBitsWidth
  val memGeneric = new xpm_memory_sdpram_generic
  memGeneric.ADDR_WIDTH_A = addressWidth
  memGeneric.ADDR_WIDTH_B = addressWidth
  memGeneric.BYTE_WRITE_WIDTH_A = wordWidth // WE为1位，暂不使用
  memGeneric.MEMORY_SIZE = wordWidth * wordCount
  memGeneric.READ_DATA_WIDTH_B = wordWidth
  memGeneric.READ_LATENCY_B = 0
  memGeneric.SIM_ASSERT_CHK = simMessage.toInt
  memGeneric.WRITE_DATA_WIDTH_A = wordWidth
  memGeneric.WRITE_MODE_B = policy.readUnderWriteString.replaceAll("([A-Z])", "_$1").toLowerCase()
  val mem = new xpm_memory_sdpram(memGeneric)
  val io = SDPRAMSyncIOBundle(wordType, wordCount)
  mem.io.read.en := True
  mem.io.read.regce := True
  mem.io.read.addr := io.read.cmd.payload

  mem.io.write.en := io.write.valid
  mem.io.write.we := mem.io.write.we.getAllTrue // 恒1
  mem.io.write.addr := io.write.payload.address
  mem.io.write.din := io.write.payload.data.asBits

  // write first
  if (outputReg) {
    io.read.rsp.setAsReg()
    when(io.read.cmd.valid) {
      when(io.write.valid && io.write.payload.address === io.read.cmd.payload) {
        io.read.rsp := io.write.payload.data
      } otherwise {
        io.read.rsp.assignFromBits(mem.io.read.dout)
      }
    }
  } else {
    when(io.write.valid && io.write.payload.address === io.read.cmd.payload) {
      io.read.rsp := io.write.payload.data
    } otherwise {
      io.read.rsp.assignFromBits(mem.io.read.dout)
    }
  }
}
