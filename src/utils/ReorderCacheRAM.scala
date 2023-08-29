package NOP.utils

import NOP.blackbox.mem._

import spinal.core._
import spinal.lib._
import scala.math

final case class RAMWriteCmdWithMask[T <: Data](
    wordType: HardType[T],
    addressWidth: Int,
    maskWidth: Int
) extends Bundle {
  val address = UInt(addressWidth bits)
  val data = wordType()
  val mask = Bits(maskWidth bits)
}

/** 读写任意连续n个数据，地址可以不对齐，不存在冲突。写可以对每个数据进行enable。
  *
  * @param wordCount
  *   Mem中总共的数据数量
  * @param portCount
  *   一次读写的数据数量
  */
class ReorderCacheRAM[T <: Data](
    dataType: HardType[T],
    wordCount: Int,
    portCount: Int,
    outputReg: Boolean = true,
    writeFirst: Boolean = false
) extends Component {
  val addressWidth = log2Up(wordCount)
  assert(isPow2(portCount))
  val offWidth = log2Up(portCount)
  assert(wordCount % portCount == 0)
  val wordsPerBank = wordCount / portCount
  require(!(writeFirst && outputReg))
  val rams =
    if (writeFirst) Seq.fill(portCount)(new SDPRAMAsyncToSyncWriteFirst(dataType, wordsPerBank))
    else Seq.fill(portCount)(new SDPRAM(dataType, wordsPerBank, outputReg))
  rams.zipWithIndex.foreach { case (ram, idx) =>
    ram.setWeakName("ram" + idx)
    OwnableRef.proposal(ram, this)
  }
  val rPorts = Vec(rams.map(_.io.read))
  val wPorts = Vec(rams.map(_.io.write))
  val io = new Bundle {
    val read = slave(MemReadPort(Vec(dataType, portCount), addressWidth))
    val write = in(Flow(RAMWriteCmdWithMask(Vec(dataType, portCount), addressWidth, portCount)))
  }
  val readLogic = new Area {
    val addrHi = io.read.cmd.payload((addressWidth - 1) downto offWidth)
    val offset = io.read.cmd.payload(0, offWidth bits)
    rPorts.zipWithIndex.map { case (p, idx) =>
      p.cmd.valid := io.read.cmd.valid
      // 例如4路，offset=2，则23均为当前地址，01为下一行
      p.cmd.payload := addrHi + (idx < offset).asUInt
    }
    // 延迟address，输出重排序
    val regAddr = Delay(offset, if (outputReg) 2 else 1, io.read.cmd.valid)
    io.read.rsp.zipWithIndex.map { case (rsp, idx) =>
      rsp := rPorts(regAddr + idx).rsp
    }
  }
  val writeLogic = new Area {
    // 可惜splitAt输出是bits tuple
    val addrHi = io.write.payload.address((addressWidth - 1) downto offWidth)
    val offset = io.write.payload.address(0, offWidth bits)
    wPorts.zipWithIndex.map { case (p, idx) =>
      p.valid := io.write.valid && io.write.payload.mask(idx - offset)
      // 同read
      p.payload.address := addrHi + (idx < offset).asUInt
      // 例如4路，offset=2，则2301 RAM分别抓0123口
      p.payload.data := io.write.payload.data(idx - offset)
    }
  }
}

class ReorderCacheRAMAsync[T <: Data](
    dataType: HardType[T],
    wordCount: Int,
    portCount: Int
) extends Component {
  val addressWidth = log2Up(wordCount)
  assert(isPow2(portCount))
  val offWidth = log2Up(portCount)
  assert(wordCount % portCount == 0)
  val wordsPerBank = wordCount / portCount
  val rams = Seq.fill(portCount)(new SDPRAMAsync(dataType, wordsPerBank))
  rams.zipWithIndex.foreach { case (ram, idx) =>
    ram.setWeakName("ram" + idx)
    OwnableRef.proposal(ram, this)
  }
  val rPorts = Vec(rams.map(_.io.read))
  val wPorts = Vec(rams.map(_.io.write))
  val io = new Bundle {
    val read = slave(MemReadPortAsync(Vec(dataType, portCount), addressWidth))
    val write = in(Flow(RAMWriteCmdWithMask(Vec(dataType, portCount), addressWidth, portCount)))
  }
  val readLogic = new Area {
    val addrHi = io.read.address((addressWidth - 1) downto offWidth)
    val offset = io.read.address(0, offWidth bits)
    rPorts.zipWithIndex.map { case (p, idx) =>
      // 例如4路，offset=2，则23均为当前地址，01为下一行
      p.address := addrHi + (idx < offset).asUInt
    }
    // 输出重排序
    io.read.data.zipWithIndex.map { case (rsp, idx) =>
      rsp := rPorts(offset + idx).data
    }
  }
  val writeLogic = new Area {
    // 可惜splitAt输出是bits tuple
    val addrHi = io.write.payload.address((addressWidth - 1) downto offWidth)
    val offset = io.write.payload.address(0, offWidth bits)
    wPorts.zipWithIndex.map { case (p, idx) =>
      p.valid := io.write.valid && io.write.payload.mask(idx - offset)
      // 同read
      p.payload.address := addrHi + (idx < offset).asUInt
      // 例如4路，offset=2，则2301 RAM分别抓0123口
      p.payload.data := io.write.payload.data(idx - offset)
    }
  }
}

class ReorderCacheRAMOutReg[T <: Data](
    dataType: HardType[T],
    wordCount: Int,
    rPortCount: Int,
    wPortCount: Int,
    outputReg: Boolean = true,
    writeFirst: Boolean = false
) extends Component {
  val addressWidth = log2Up(wordCount)
  val offWidth = log2Up(math.max(rPortCount, wPortCount))
  val bankCount = 1 << offWidth
  assert(wordCount % bankCount == 0)
  val wordsPerBank = wordCount / bankCount
  require((writeFirst && !outputReg) || (!writeFirst && outputReg))
  val rams =
    if (writeFirst)
      Seq.fill(bankCount)(new SDPRAMAsyncToSyncWriteFirst(dataType, wordsPerBank, false))
    else Seq.fill(bankCount)(new SDPRAM(dataType, wordsPerBank, false))
  rams.zipWithIndex.foreach { case (ram, idx) =>
    ram.setWeakName("ram" + idx)
    OwnableRef.proposal(ram, this)
  }
  val rPorts = Vec(rams.map(_.io.read))
  val wPorts = Vec(rams.map(_.io.write))
  val io = new Bundle {
    val read = slave(MemReadPort(Vec(dataType, rPortCount), addressWidth))
    val write = in(Flow(RAMWriteCmdWithMask(Vec(dataType, wPortCount), addressWidth, wPortCount)))
  }
  io.read.rsp.setAsReg()
  val readLogic = new Area {
    val addrHi = io.read.cmd.payload((addressWidth - 1) downto offWidth)
    val offset = io.read.cmd.payload(0, offWidth bits)
    rPorts.zipWithIndex.map { case (p, idx) =>
      p.cmd.valid := io.read.cmd.valid
      // 例如4路，offset=2，则23均为当前地址，01为下一行
      p.cmd.payload := addrHi + (idx < offset).asUInt
    }
    // 延迟address，输出重排序
    val regAddr = Delay(offset, if (outputReg) 1 else 0, io.read.cmd.valid)
    io.read.rsp.zipWithIndex.map { case (rsp, idx) =>
      rsp := rPorts(regAddr + idx).rsp
    }
  }
  val writeLogic = new Area {
    // 可惜splitAt输出是bits tuple
    val addrHi = io.write.payload.address((addressWidth - 1) downto offWidth)
    val offset = io.write.payload.address(0, offWidth bits)
    wPorts.zipWithIndex.map { case (p, idx) =>
      val writeMask = False
      val writeData = dataType().assignDontCare()
      for (i <- 0 until wPortCount) when(offset === (idx - i + bankCount) % bankCount) {
        writeMask := io.write.payload.mask(i)
        writeData := io.write.payload.data(i)
      }
      p.valid := io.write.valid && writeMask
      // 同read
      p.payload.address := addrHi + (idx < offset).asUInt
      // 例如4路，offset=2，则2301 RAM分别抓0123口 (idx-offset)
      p.payload.data := writeData
    }
  }
}
