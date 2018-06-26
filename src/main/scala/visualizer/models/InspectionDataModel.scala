package visualizer.models

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Transition(timestamp : Long, value : BigInt)

case class Waveform(name : String, transitions : ArrayBuffer[Transition])


class InspectionDataModel {
  val allWaves = new mutable.HashMap[String, Waveform]()
}
