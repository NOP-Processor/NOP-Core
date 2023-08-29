package NOP.pipeline.fetch

import NOP.pipeline._
import NOP.builder._
import NOP.utils._
import NOP._
import NOP.pipeline.core.CommitPlugin
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class ReturnAddressStackPlugin(config: FrontendConfig) extends Plugin[FetchPipeline] {
  require(isPow2(config.btb.rasEntries))
  val ras = Vec(RegInit(U(config.pcInit >> 2, 30 bits)), config.btb.rasEntries)
  val rasTopEntry = RegInit(U(0, log2Up(config.btb.rasEntries) bits))
  val rasPredict = Flow(UWord()).setIdle()

  override def build(pipeline: FetchPipeline): Unit = {
    pipeline.IF2 plug new Area {

      import pipeline.IF2._
      import pipeline.signals._

      val jumpFlag = input(PREDICT_JUMP_FLAG)
      val jumpPayload = input(PREDICT_JUMP_PAYLOAD)
      val jumpWay = input(PREDICT_JUMP_WAY)

      insert(RECOVER_TOP) := rasTopEntry // set Recover top for every instruction

      when(arbitration.isValid && !arbitration.isStuck && jumpFlag) { // update ras when call or return is found
        // note that ras shouldn't be updated when fetching delay slot
        // note that ras predict update logic is triggered only when valid and firing
        when(jumpPayload.isCall) {
          ras(rasTopEntry + 1) := input(PC)(2, 30 bits) + jumpWay + 1
          rasTopEntry := rasTopEntry + 1
        } elsewhen (jumpPayload.isReturn) {
          rasTopEntry := rasTopEntry - 1
          rasPredict.push(ras(rasTopEntry) @@ U(0, 2 bits))
        }
      }
    }

    pipeline plug new Area {
      // commit area
      val bpuCommit = pipeline.globalService(classOf[CommitPlugin])
      val predUpdate = bpuCommit.predUpdate

      when(predUpdate.valid) { // time to update
        val payload = predUpdate.payload
        val pred = payload.predInfo
        val recover = payload.predRecover
        when(pred.predictBranch && !payload.branchLike) {
          // Case 1, modified instruction, not branch anymore, must be mispredicted
          rasTopEntry := recover.recoverTop
        }

        when(payload.mispredict) { // ras is the only thing need to recover when mispredict
          when(payload.isCall) {
            rasTopEntry := recover.recoverTop + 1
            ras(recover.recoverTop + 1) := payload.pc(31 downto 2) + 1
          } elsewhen (payload.isRet) {
            rasTopEntry := recover.recoverTop - 1
          } otherwise {
            rasTopEntry := recover.recoverTop
          }
        }
      }
    }
  }
}
