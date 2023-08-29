package NOP.pipeline.core

import spinal.core._
import spinal.lib._
import NOP.constants.LoongArch
import NOP.utils._
import NOP.builder._
import scala.collection.mutable
import scala.math

class ExceptionMuxPlugin[T <: Pipeline](lastInsertIdx: Int) extends Plugin[T] {
  case class ExceptionSource(valid: Bool, badVAddr: UInt, exc: LoongArch.ExceptionCode.Exception, priority: Int)
  private val excMap = mutable.HashMap[Stage, mutable.ArrayBuffer[ExceptionSource]]()
  def addExceptionSource(
      stage: Stage,
      exc: LoongArch.ExceptionCode.Exception,
      valid: Bool,
      badVAddr: UInt = null,
      priority: Int = 0
  ) {
    excMap.getOrElseUpdate(stage, mutable.ArrayBuffer()) +=
      ExceptionSource(valid, badVAddr, exc, priority)
  }

  object ExceptionSignals {
    object BAD_VADDR extends Stageable(UWord())
    object EXCEPTION_ECODE extends Stageable(Bits(6 bits))
    object EXCEPTION_ESUBCODE extends Stageable(Bits(9 bits))
    object EXCEPTION_OCCURRED extends Stageable(Bool())
  }

  override def build(pipeline: T): Unit = pipeline plug new Area {

    import ExceptionSignals._

    // insert exception signals for handler, even if no exception happens
    var firstExcStage = lastInsertIdx
    var firstVAStage = lastInsertIdx
    excMap.foreach { case (stage, excList) =>
      if (excList.nonEmpty)
        firstExcStage = math.min(firstExcStage, pipeline.indexOf(stage))
      excList.sortBy(_.priority).foreach { excSrc =>
        stage.output(EXCEPTION_OCCURRED).setWhen(excSrc.valid)
        // 如果先前已经有异常，那么忽略本阶段产生的异常
        when(!stage.input(EXCEPTION_OCCURRED) && excSrc.valid) {
          stage.output(EXCEPTION_ECODE) := U(excSrc.exc.ecode, 6 bits).asBits
          stage.output(EXCEPTION_ESUBCODE) := U(excSrc.exc.esubcode, 9 bits).asBits
          // badVA仅在需要时修改
          if (excSrc.badVAddr != null) {
            firstVAStage = math.min(firstVAStage, pipeline.indexOf(stage))
            stage.output(BAD_VADDR) := excSrc.badVAddr
          }
        }
      }
    }
    // 初值在需要的阶段insert
    pipeline.stages(firstExcStage).insert(EXCEPTION_OCCURRED) := False
    pipeline.stages(firstExcStage).insert(EXCEPTION_ECODE).assignDontCare()
    pipeline.stages(firstExcStage).insert(EXCEPTION_ESUBCODE).assignDontCare()
    pipeline.stages(firstVAStage).insert(BAD_VADDR).assignDontCare()
  }
}
