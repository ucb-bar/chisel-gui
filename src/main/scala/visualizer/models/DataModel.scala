package visualizer.models

import treadle.vcd.VCD
import visualizer.ChiselGUI.dataModel
import visualizer.{ChiselGUI, MaxTimestampChanged}

import scala.collection.Searching._
import scala.collection.mutable
import scala.swing.Publisher

/** This is the general model for all data relating to signals and their
  * data values. Ground truth resides in the VCD in TreadleController
  *
  */
class DataModel extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model manages the list of signals
  ///////////////////////////////////////////////////////////////////////////
  val nameToSignal = new mutable.HashMap[String, Signal]

  def addSignal(fullName: String, signal: Signal): Unit = {
    dataModel.nameToSignal(fullName) = signal
  }

  /** Call this if the vcd has changed in some way
    *
    */
  def loadMoreWaveformValues(): Unit = {
    ChiselGUI.testerOpt.foreach { t =>
      t.engine.vcdOption.foreach {
        case vcd: VCD =>
          Waves.refreshWaves(vcd)
          dataModel.setMaxTimestamp(vcd.timeStamp)

          dataModel.nameToSignal.values.foreach {
            case decoupledSignalGroup: DecoupledSignalGroup =>
              decoupledSignalGroup.updateValues()
            case validSignalGroup: ValidSignalGroup =>
              validSignalGroup.updateValues()
            case _ =>
          }
        case _ =>
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Input panel stuff
  ///////////////////////////////////////////////////////////////////////////

  val pokeHistory: mutable.ArrayBuffer[Map[String, String]] = new mutable.ArrayBuffer()
  var currentPokeHistoryIndex = 0

  def savePokeValues(pokeData: Map[String, String]): Unit = {
    pokeHistory.lastOption match {
      case Some(lastPokeData) =>
        val thereWereChanges = (lastPokeData.keys ++ pokeData.keys).exists { key =>
          lastPokeData.getOrElse(key, "") != pokeData.getOrElse(key, "")
        }
        if (thereWereChanges) {
          pokeHistory += pokeData
          currentPokeHistoryIndex = pokeHistory.length - 1
        }
      case _ =>
        pokeHistory += pokeData
        currentPokeHistoryIndex = pokeHistory.length - 1
    }
    if (pokeHistory.length > 50) {
      // Throw away the old stuff
      pokeHistory.remove(0, pokeHistory.length - 50)
    }
  }

  def grabInputs(time: Long): Map[String, BigInt] = {
    val inputMap = new mutable.HashMap[String, BigInt]

    ChiselGUI.testerOpt.foreach { t =>
      val inputNames = t.engine.getInputPorts.toSet
      val numInputs = t.engine.getInputPorts.length

      t.engine.vcdOption.foreach {
        case vcd: VCD =>
          val eventTimes = vcd.events

          var index = vcd.events.search(time) match {
            case InsertionPoint(insertionPointIndex) => insertionPointIndex
            case Found(index)                        => index + 1
            case _                                   => -1
          }

          while (index > 0 && inputMap.size < numInputs) {
            index -= 1
            vcd.valuesAtTime(eventTimes(index)).foreach { change =>
              val signalName = change.wire.fullName
              if (inputNames.contains(signalName)) {
                if (!inputMap.contains(signalName)) {
                  inputMap(signalName) = change.value
                }
              }
            }
          }
        case _ =>
      }
    }

    inputMap.toMap
  }

  ///////////////////////////////////////////////////////////////////////////
  // Timescale and Max Timestamp
  ///////////////////////////////////////////////////////////////////////////
  var maxTimestamp: Long = 0L

  def setMaxTimestamp(value: Long): Unit = {
    if (value > maxTimestamp) {
      maxTimestamp = value
      publish(new MaxTimestampChanged)
    }
  }

  var timescale: Int = -9
}
