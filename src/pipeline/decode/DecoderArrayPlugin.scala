package NOP.pipeline.decode

import spinal.core._
import spinal.lib._

import scala.collection._
import NOP.constants.enum._
import NOP.pipeline.decode._
import NOP.pipeline.fetch._
import NOP.pipeline._
import NOP.builder._
import NOP.constants.LoongArch.{ExceptionCode, TLBRD}
import NOP.pipeline.priviledge.ExceptionHandlerPlugin
import NOP.utils._
import NOP.{constants, _}

import scala.annotation.meta.field

/** Decoder service.
  *
  * @param config
  * @param popPorts
  *   Pop ports of fetch buffer.
  */
class DecoderArray(config: MyCPUConfig, val popPorts: Vec[Stream[InstBufferEntry]]) extends Plugin[DecodePipeline] {
  private val width = config.decode.decodeWidth
  private val defaults =
    mutable.LinkedHashMap[Stageable[_ <: Data], Data]()
  private val encodings =
    mutable.LinkedHashMap[MaskedLiteral, mutable.ArrayBuffer[(Stageable[_ <: Data], Data)]]()
  private val priorities = mutable.LinkedHashMap[MaskedLiteral, Int]()
  private val stageables = mutable.HashSet[Stageable[_ <: Data]]()

  /** 添加多个指令编码。
    *
    * @param encoding
    *   指令编码序列
    */
  def addAll(
      encodings: (MaskedLiteral, Seq[(Stageable[_ <: Data], Any)])*
  ): Unit = encodings.foreach(e => this.add(e._1, e._2))

  /** 添加一条指令编码。
    *
    * @param key
    *   指令编码
    * @param values
    *   解码信号值
    */
  def add(
      key: MaskedLiteral,
      values: Seq[(Stageable[_ <: Data], Any)]
  ): Unit = {
    if (encodings.contains(key))
      println(s"Warning: already added encoding ${key.getBitsString(32, '-')}")
    val instructionModel = encodings.getOrElseUpdate(key, mutable.ArrayBuffer())
    values.map { case (a, b) =>
      assert(!instructionModel.contains(a), s"Over specification of $a")
      b match {
        case value: Data                 => instructionModel += (a -> value)
        case value: SpinalEnumElement[_] => instructionModel += (a -> value())
        case _                           => throw new AssertionError(b.getClass().getName())
      }
    }
  }

  /** 设置默认解码。
    *
    * @param key
    *   信号key
    * @param value
    *   信号默认值
    */
  def setDefault[T <: Data](key: Stageable[T], value: T): Unit = {
    assert(!defaults.contains(key))
    defaults(key) = value
  }
  def setDefault[T <: SpinalEnum](
      key: Stageable[SpinalEnumCraft[T]],
      value: SpinalEnumElement[T]
  ): Unit = {
    assert(!defaults.contains(key))
    defaults(key) = value()
  }

  private def setDefaultDontCare[T <: Data](key: Stageable[T]): Unit =
    setDefault(key, key().assignDontCare())

  /** 设置默认解码为don't care。
    *
    * @param key
    *   信号key
    */
  def defaultDontCare(keys: Stageable[_ <: Data]*) {
    keys.foreach(setDefaultDontCare(_))
  }

  /** 设置默认值为false。
    *
    * @param keys
    */
  def defaultFalse(keys: Stageable[Bool]*) {
    keys.foreach(setDefault(_, False))
  }

  /** 设置解码优先级。用于一个编码下的子编码（例如SSNOP）
    *
    * @param key
    *   指令编码
    * @param priority
    *   优先级，越大优先级越高
    */
  def setPriority(key: MaskedLiteral, priority: Int) {
    priorities(key) = priority
  }

  // Build array of decoder
  override def build(pipeline: DecodePipeline): Unit = {

    BuildLoongArch(pipeline)

    pipeline.ID plug new Area {
      import pipeline.ID._

      // val excHandler = pipeline.globalService[ExceptionHandler]
      // build stageable list
      stageables ++= encodings.flatMap(_._2.map(_._1))
      val noDefaultList = mutable.ArrayBuffer[Stageable[_ <: Data]]()
      for (s <- stageables) {
        if (!defaults.contains(s)) {
          noDefaultList += s
        }
      }
      if (!noDefaultList.isEmpty) {
        val msg = noDefaultList.mkString(", ") + " have no default decode value"
        throw new Exception(msg)
      }
      for (s <- defaults.keys) {
        if (!stageables.contains(s)) {
          println(
            s"Warning: ${s.getName()} has default value but is never used in decoding"
          )
        }
        stageables += s
      }

      val normalEncodings =
        mutable.ArrayBuffer[(MaskedLiteral, mutable.ArrayBuffer[(Stageable[_ <: Data], Data)])]()
      val priorEncodings = mutable.ArrayBuffer[(Int, MaskedLiteral)]()
      for ((encoding, actions) <- encodings) {
        priorities.get(encoding) match {
          case Some(value) => priorEncodings += (value -> encoding)
          case None        => normalEncodings += (encoding -> actions)
        }
      }
      val encodingsByPriority = priorEncodings.sortBy(_._1).map(_._2)
      println(
        s"Decoder array: ${normalEncodings.size} normal encoding(s) and ${priorEncodings.size} priority encoding(s)"
      )

      def buildSingleDecoder(input: InstBufferEntry) =
        new Composite(input) {
          val entry = input
          val illegalEncoding = True // Flag if the instruction is illegal, i.e. no encoding matches

          val fields = InstructionParser(entry.inst) // Parse

          val locals = mutable.LinkedHashMap[Stageable[Data], Data]()
          def local[T <: Data](key: Stageable[T]): T = locals
            .getOrElseUpdate(key.asInstanceOf[Stageable[Data]], key())
            .asInstanceOf[T] // If not exists, initialize a LOCAL Stageable (not in uop)

          // ! Prologue: Default values for the uop (which is the return value of this decoder)
          val uop = MicroOp(config)
          private val uopElements = uop.elements.toMap // Map[String,Data]
          for ((k, v) <- defaults) uopElements.get(k.getName()) match {
            case Some(value) => value.assignFrom(v)
            case None        => local(k).assignFrom(v)
          }

          // Func for decoding instruction.
          def decodeInstruction(
              encoding: MaskedLiteral,
              actions: Seq[(Stageable[_ <: Data], Data)]
          ) {
            when(entry.inst === encoding) {
              // 如果要增加Verilog可读性，则通过类似Stageable的方式获取编码的指令名，给when条件setName
              // 全覆盖的错误可以通过assignment overlap检查出（通常出现于复制忘了改）
              illegalEncoding.clear()
              for ((k, v) <- actions) uopElements.get(k.getName()) match {
                case Some(value) => value.assignFrom(v)
                case None        => local(k).assignFrom(v)
              }
            }
          }

          // ! Main body for Decoding instructions
          for ((encoding, actions) <- normalEncodings) decodeInstruction(encoding, actions)
          // priority升序，when语句后面的覆盖前面的
          for (encoding <- encodingsByPriority) decodeInstruction(encoding, encodings(encoding))

          // ! Epilogue: override some fields
          import NOP.constants.LoongArch
          import MicroOpSignals._
          // Start Assigning retval (uop)
          uop.pc := entry.pc
          uop.inst := entry.inst

          // Branch Prediction
          uop.predInfo := entry.predInfo
          uop.predRecover := entry.predRecover

          // CSR
          uop.writeCSR := uop.readCSR && (fields.rj =/= 0)
          uop.useRd.allowOverride()
          when(uop.writeCSR) {
            uop.useRd.set()
          }
          uop.useRj.allowOverride()
          when(uop.writeCSR && fields.rj =/= 1) {
            uop.useRj.set()
          }

          // Timer
          uop.readTimer64L.clear()
          uop.readTimer64ID.clear()
          when(uop.fuType === FUType.TIMER && ~uop.readTimer64H) {
            when(fields.rd === 0) {
              local(overrideRdToRj).set()
              uop.readTimer64ID.set()
            } otherwise {
              uop.readTimer64L.set()
            }
          }

          // REG W
          when(local(overrideRdToRA)) {
            uop.wbAddr := U(1, config.regFile.arfAddrWidth bits)
          } otherwise {
            when(local(overrideRdToRj)) {
              uop.wbAddr := fields.rj
            } otherwise {
              uop.wbAddr := fields.rd
            }
          }
          uop.doRegWrite := local(isRegWrite) & (uop.wbAddr =/= 0)

          // decode异常
          uop.except.valid := False
          uop.except.allowOverride()
          uop.except := entry.except
          when(!entry.except.valid) {
            uop.except.payload.isTLBRefill := False
            when(local(isSyscall)) {
              uop.except.valid := True
              uop.except.payload.code := ExceptionCode.SYS.ecode
              uop.except.payload.subcode := ExceptionCode.SYS.esubcode
            }
            when(local(isBreak)) {
              uop.except.valid := True
              uop.except.payload.code := ExceptionCode.BRK.ecode
              uop.except.payload.subcode := ExceptionCode.BRK.esubcode
            }
            when(illegalEncoding) {
              uop.except.valid := True
              uop.except.payload.code := ExceptionCode.INE.ecode
              uop.except.payload.subcode := ExceptionCode.INE.esubcode
            }

            // TLB
            when(LoongArch.INVTLB === entry.inst && uop.useRj && uop.useRk) {
              // INVTLB
              val opCode = entry.inst(4 downto 0)
              switch(opCode) {
                is(0, 1) {
                  uop.tlbOp := TLBOpType.INVTLB1
                }
                is(2) {
                  uop.tlbOp := TLBOpType.INVTLB2
                }
                is(3) {
                  uop.tlbOp := TLBOpType.INVTLB3
                }
                is(4) {
                  uop.tlbOp := TLBOpType.INVTLB4
                }
                is(5) {
                  uop.tlbOp := TLBOpType.INVTLB5
                }
                is(6) {
                  uop.tlbOp := TLBOpType.INVTLB6
                }
                default {
                  uop.except.valid := True
                  uop.except.payload.code := ExceptionCode.INE.ecode
                  uop.except.payload.subcode := ExceptionCode.INE.esubcode
                }
              }
            }

            // IPE
            import NOP.constants.LoongArch
            val privInst =
              entry.inst === LoongArch.CSR || entry.inst === LoongArch.CACOP || entry.inst === LoongArch.ERTN ||
                entry.inst === LoongArch.IDLE || entry.inst === LoongArch.TLBRD || entry.inst === LoongArch.TLBWR ||
                entry.inst === LoongArch.TLBSRCH || entry.inst === LoongArch.TLBFILL || entry.inst === LoongArch.INVTLB

            val privLvl = pipeline.globalService(classOf[ExceptionHandlerPlugin]).CRMD_PLV
            when(privLvl =/= 0 && privInst) {
              uop.except.valid := True
              uop.except.payload.code := ExceptionCode.IPE.ecode
              uop.except.payload.subcode := ExceptionCode.IPE.esubcode
            }
          }

          uop.branchLike := uop.isBranch || uop.isJump || uop.isJR
          // 处理unique retire类指令
          uop.flushState := uop.isErtn || uop.writeCSR || uop.isWait ||
            uop.operateTLB || uop.operateCache || local(isBar) || uop.isLL || uop.isSC
          // flushState 是 uniqueRetire 的子集

          if (config.decode.allUnique) {
            println("[Warning] All Unique retire enabled")
            uop.uniqueRetire := True
          } else {
            uop.uniqueRetire := uop.flushState ||
              uop.branchLike ||
              uop.isLoad ||
              uop.isStore ||
              uop.predInfo.predictBranch
          }
        }

      // 生成decoder阵列
      val decoders = for (i <- 0 until width) yield buildSingleDecoder(popPorts(i).payload)
      val decodePacket = insert(pipeline.signals.DECODE_PACKET)
      for (i <- 0 until width) {
        decodePacket(i).valid := popPorts(i).valid
        decodePacket(i).payload := decoders(i).uop
        popPorts(i).ready := arbitration.isFiring
      }

    }
  }

  def BuildLoongArch(pipeline: DecodePipeline): Unit = {
    import NOP.constants.LoongArch
    val decoder = pipeline.service(classOf[DecoderArray])

    import MicroOpSignals._

    // 保持与micro op一样的顺序
    setDefault(fuType, FUType.ALU)
    setDefault(tlbOp, TLBOpType.NONE)
    setDefault(operateCache, False)
    defaultFalse(useRj, useRk, useRd, isRegWrite, overrideRdToRA, overrideRdToRj)
    defaultFalse(readCSR, readTimer64H)
    setDefault(immExtendType, ExtendType.SIGN)
    defaultDontCare(aluOp, cmpOp, signed)
    setDefault(lsType, LoadStoreType.BYTE)
    defaultFalse(isLoad, isStore, isLL, isSC)
    defaultFalse(isBranch, isJump, isJR)
    defaultFalse(isSyscall, isErtn, isBreak, isWait, isBar)

    // These actions are common to all instructions
    // Therefore, they are defined here, which can be reused by all instructions
    val reg1WActions = Seq(isRegWrite -> True)
    val reg1RActions = Seq(useRj -> True)
    val reg1R1WActions = reg1RActions ++ Seq(isRegWrite -> True)
    val reg2RActions = Seq(useRj -> True, useRk -> True)
    val reg2R1WActions = reg2RActions ++ Seq(isRegWrite -> True)
    val useUnsignedImm = Seq(immExtendType -> ExtendType.ZERO)

    // Integer
    addAll(
      LoongArch.ADD -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.ADD)),
      LoongArch.SUB -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.SUB)),
      LoongArch.SLT -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.SLT)),
      LoongArch.SLTU -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.SLTU)),
      LoongArch.NOR -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.NOR)),
      LoongArch.AND -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.AND)),
      LoongArch.OR -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.OR)),
      LoongArch.XOR -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.XOR)),
      LoongArch.SLL -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.SLL)),
      LoongArch.SRL -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.SRL)),
      LoongArch.SRA -> (reg2R1WActions ++ Seq(aluOp -> ALUOpType.SRA))
    )

    // Integer Immediate
    addAll(
      LoongArch.SLLI -> (reg1R1WActions ++ Seq(aluOp -> ALUOpType.SLL)),
      LoongArch.SRLI -> (reg1R1WActions ++ Seq(aluOp -> ALUOpType.SRL)),
      LoongArch.SRAI -> (reg1R1WActions ++ Seq(aluOp -> ALUOpType.SRA)),
      LoongArch.SLTI -> (reg1R1WActions ++ Seq(aluOp -> ALUOpType.SLT)),
      LoongArch.SLTUI -> (reg1R1WActions ++ Seq(aluOp -> ALUOpType.SLTU)),
      LoongArch.ADDI -> (reg1R1WActions ++ Seq(aluOp -> ALUOpType.ADD)),
      LoongArch.ANDI -> (useUnsignedImm ++ reg1R1WActions ++ Seq(aluOp -> ALUOpType.AND)),
      LoongArch.ORI -> (useUnsignedImm ++ reg1R1WActions ++ Seq(aluOp -> ALUOpType.OR)),
      LoongArch.XORI -> (useUnsignedImm ++ reg1R1WActions ++ Seq(aluOp -> ALUOpType.XOR))
    )

    addAll(
      LoongArch.LU12I -> (reg1WActions ++ Seq(aluOp -> ALUOpType.LU12I)),
      LoongArch.PCADDI -> (reg1WActions ++ Seq(aluOp -> ALUOpType.PCADDI)),
      LoongArch.PCADDU12I -> (reg1WActions ++ Seq(aluOp -> ALUOpType.PCADDU12I))
    )

    // Branch
    // ! Note that branch instructions use Rj (0) and **Rd** (1)
    val branchActions = Seq(fuType -> FUType.CMP, isBranch -> True, useRd -> True)
    addAll(
      LoongArch.BEQ -> (branchActions ++ reg1RActions ++ Seq(cmpOp -> CompareOpType.EQ)),
      LoongArch.BNE -> (branchActions ++ reg1RActions ++ Seq(cmpOp -> CompareOpType.NE)),
      LoongArch.BLT -> (branchActions ++ reg1RActions ++ Seq(cmpOp -> CompareOpType.LT)),
      LoongArch.BGE -> (branchActions ++ reg1RActions ++ Seq(cmpOp -> CompareOpType.GE)),
      LoongArch.BLTU -> (branchActions ++ reg1RActions ++ Seq(cmpOp -> CompareOpType.LTU)),
      LoongArch.BGEU -> (branchActions ++ reg1RActions ++ Seq(cmpOp -> CompareOpType.GEU))
    )

    // Jump
    val jumpActions = Seq(fuType -> FUType.CMP, isJump -> True)
    val jumpLinkActions = Seq(fuType -> FUType.CMP, isJR -> True, useRj -> True)
    addAll(
      LoongArch.B -> jumpActions,
      LoongArch.BL -> (jumpActions ++ Seq(isRegWrite -> True, overrideRdToRA -> True)),
      LoongArch.JIRL -> (jumpLinkActions ++ Seq(isRegWrite -> True))
    )

    // Mul or Div
    addAll(
      LoongArch.MUL -> (reg2R1WActions ++ Seq(fuType -> FUType.MUL, signed -> True)),
      LoongArch.MULH -> (reg2R1WActions ++ Seq(fuType -> FUType.MULH, signed -> True)),
      LoongArch.MULHU -> (reg2R1WActions ++ Seq(fuType -> FUType.MULH, signed -> False)),
      LoongArch.DIV -> (reg2R1WActions ++ Seq(fuType -> FUType.DIV, signed -> True)),
      LoongArch.DIVU -> (reg2R1WActions ++ Seq(fuType -> FUType.DIV, signed -> False)),
      LoongArch.MOD -> (reg2R1WActions ++ Seq(fuType -> FUType.MOD, signed -> True)),
      LoongArch.MODU -> (reg2R1WActions ++ Seq(fuType -> FUType.MOD, signed -> False))
    )

    // Load or store
    val memActions = Seq[(Stageable[_ <: Data], Any)](fuType -> FUType.LSU)
    val loadActions = memActions ++ reg1R1WActions ++ Seq(isLoad -> True)
    val storeActions = memActions ++ reg1RActions ++ Seq(isStore -> True, useRd -> True)
    addAll(
      LoongArch.LDW -> (loadActions ++ Seq(lsType -> LoadStoreType.WORD)),
      LoongArch.LDH -> (loadActions ++ Seq(lsType -> LoadStoreType.HALF)),
      LoongArch.LDHU -> (loadActions ++ Seq(lsType -> LoadStoreType.HALF_U)),
      LoongArch.LDB -> (loadActions ++ Seq(lsType -> LoadStoreType.BYTE)),
      LoongArch.LDBU -> (loadActions ++ Seq(lsType -> LoadStoreType.BYTE_U)),
      LoongArch.STW -> (storeActions ++ Seq(lsType -> LoadStoreType.WORD)),
      LoongArch.STH -> (storeActions ++ Seq(lsType -> LoadStoreType.HALF)),
      LoongArch.STB -> (storeActions ++ Seq(lsType -> LoadStoreType.BYTE))
    )
    addAll(
      LoongArch.LL -> Seq(
        fuType -> FUType.LSU,
        useRj -> True,
        isRegWrite -> True,
        isLoad -> True,
        lsType -> LoadStoreType.WORD,
        isLL -> True
      ),
      LoongArch.SC -> Seq(
        fuType -> FUType.LSU,
        useRj -> True,
        useRd -> True,
        isRegWrite -> True,
        isStore -> True,
        lsType -> LoadStoreType.WORD,
        isSC -> True
      )
    )

    // CSR
    addAll(
      LoongArch.CSR -> Seq(fuType -> FUType.CSR, isRegWrite -> True, readCSR -> True)
    )

    // Timer
    addAll(
      LoongArch.RDCNTVL -> Seq(fuType -> FUType.TIMER, isRegWrite -> True),
      LoongArch.RDCNTVH -> Seq(fuType -> FUType.TIMER, isRegWrite -> True, readTimer64H -> True)
    )

    addAll(
      LoongArch.SYSCALL -> Seq(fuType -> FUType.NONE, isSyscall -> True),
      LoongArch.BREAK -> Seq(fuType -> FUType.NONE, isBreak -> True),
      LoongArch.ERTN -> Seq(fuType -> FUType.NONE, isErtn -> True),
      LoongArch.IDLE -> Seq(fuType -> FUType.NONE, isWait -> True)
    )

    addAll(
      LoongArch.TLBSRCH -> Seq(fuType -> FUType.NONE, tlbOp -> TLBOpType.TLBSRCH),
      LoongArch.TLBRD -> Seq(fuType -> FUType.NONE, tlbOp -> TLBOpType.TLBRD),
      LoongArch.TLBWR -> Seq(fuType -> FUType.NONE, tlbOp -> TLBOpType.TLBWR),
      LoongArch.TLBFILL -> Seq(fuType -> FUType.NONE, tlbOp -> TLBOpType.TLBFILL),
      LoongArch.INVTLB -> (Seq(useRj -> True, useRk -> True, fuType -> FUType.INVTLB, tlbOp -> TLBOpType.INVTLB1))
    )

    addAll(
      LoongArch.IBAR -> Seq(fuType -> FUType.NONE, isBar -> True),
      LoongArch.DBAR -> Seq(fuType -> FUType.NONE, isBar -> True)
    )

    addAll(
      LoongArch.CACOP -> Seq(fuType -> FUType.LSU, operateCache -> True, useRj -> True, lsType -> LoadStoreType.CACOP)
    )
  }

}
