package visualizer.models

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Transition(timestamp : Long, value : BigInt)

case class Waveform(name : String, transitions : ArrayBuffer[Transition])


class InspectionDataModel {
  val allWaves = new mutable.HashMap[String, Waveform]()

  var maxTimestamp: Long = 0

  def updateMaxTimestamp : Unit = {
    maxTimestamp = allWaves.values.map { w => w.transitions(w.transitions.size - 1).timestamp }.max
    println(s"new max timestamp: $maxTimestamp")
  }
}