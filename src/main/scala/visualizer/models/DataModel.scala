package visualizer.models

import treadle.vcd.VCD
import visualizer.ChiselGUI.dataModel
import visualizer.{ChiselGUI, MaxTimestampChanged, Util}

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
          dataModel.setMaxTimestamp(vcd.events.last)

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

    // This updates the signals that are combinations of pure signals
    // Todo: Make this more general
    dataModel.nameToSignal.values.foreach {
      case decoupledSignalGroup: DecoupledSignalGroup =>
        decoupledSignalGroup.updateValues()
      case validSignalGroup: ValidSignalGroup =>
        validSignalGroup.updateValues()
      case _ =>
    }
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
