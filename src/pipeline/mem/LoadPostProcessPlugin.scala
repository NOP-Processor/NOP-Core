package NOP.pipeline.mem

import spinal.core._
import spinal.lib._
import NOP.utils._
import NOP.constants.enum._
import NOP.pipeline.core._
import NOP.builder._
import NOP.pipeline._
import NOP.pipeline.decode._
import NOP._
import NOP.pipeline.exe.MemIssueQueuePlugin

/** 拼出输出word。
  */
class LoadPostprocessPlugin extends Plugin[MemPipeline] {
  override def build(pipeline: MemPipeline): Unit = pipeline.MEM2 plug new Area {
    import pipeline.MEM2._
    import pipeline.signals._
    val std = input(STD_SLOT)
    val accessType = LoadStoreType()
    // 这里用VA或PA都一样
    val wordOffset = UInt(2 bits)
    val rtData = BWord()
    val readWord = input(MEMORY_READ_DATA)
    val wbWord = output(MEMORY_READ_DATA).allowOverride
    when(std.valid) {
      // 必然不与load一起，LDU需要load postprocess
      accessType := std.lsType
      wordOffset := std.addr(1 downto 0)
      rtData := std.data
    } otherwise {
      // load使用load postprocess
      accessType := input(LOAD_STORE_TYPE)
      // 这里用VA或PA都一样
      wordOffset := input(MEMORY_ADDRESS_PHYSICAL)(1 downto 0)
      rtData := input(MEMORY_WRITE_DATA)
    }
    switch(accessType) {
      import LoadStoreType._
      is(BYTE) {
        wbWord := wordOffset
          .muxList(Seq.tabulate(4) { i => i -> readWord(i * 8, 8 bits) })
          .asSInt
          .resize(32 bits)
          .asBits
      }
      is(BYTE_U) {
        wbWord := wordOffset
          .muxList(Seq.tabulate(4) { i => i -> readWord(i * 8, 8 bits) })
          .asUInt
          .resize(32 bits)
          .asBits
      }
      is(HALF) {
        wbWord := Mux(wordOffset(1), readWord(16, 16 bits), readWord(0, 16 bits)).asSInt
          .resize(32 bits)
          .asBits
      }
      is(HALF_U) {
        wbWord := Mux(wordOffset(1), readWord(16, 16 bits), readWord(0, 16 bits)).asUInt
          .resize(32 bits)
          .asBits
      }
      is(WORD) {
        wbWord := readWord
      }
    }
  }
}
