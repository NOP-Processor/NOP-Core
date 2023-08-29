package NOP.utils

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4

object Axi4Rename {

  def Rename(bundle: Bundle): Unit = {
    bundle.elements.foreach {
      case (name, ref) => {
        // println(s"Name: $name, Ref: $ref")
        ref match {
          case b: Axi4 => {
            b.flattenForeach { signal =>
              {
                // Replace `_` and `payload` with nothing
                signal.setName(signal.getName().replace("_payload_", ""))
                signal.setName(signal.getName().replace("_", ""))
              }
            }
            // print(b)
          }
          case _ =>
        }
      }
    }
  }

}
