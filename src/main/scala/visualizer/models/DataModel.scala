package visualizer.models

import visualizer.ChiselGUI.dataModel
import visualizer.{MaxTimestampChanged, ChiselGUI, Util}

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
  val nameToSignal = new mutable.HashMap[String, Signal[_]]

  def ioSignals: Seq[String] = {
    val a = nameToSignal.flatMap {
      case (fullName: String, pureSignal: PureSignal) =>
        if (pureSignal.sortGroup == 0) Some(fullName) else None
      case _ =>
        None
    }
    a.toSeq.sorted
  }

  def addSignal(fullName: String, signal: Signal[_ <: Any]): Unit = {
    dataModel.nameToSignal(fullName) = signal
  }

  /** Call this if the vcd has changed in some way
    *
    */
  def loadMoreWaveformValues(): Unit = {
    ChiselGUI.testerOpt match {
      case Some(t) =>
        t.engine.vcdOption match {
          case Some(vcd) =>
            Util.vcdToTransitions(vcd, initializing = true).foreach {
              case (fullName, transitions) =>
                nameToSignal.get(fullName) match {
                  case Some(pureSignal: PureSignal) =>
                    pureSignal.addNewValues(transitions)
                  case Some(combinedSignal: CombinedSignal) =>
                  //TODO: figure out if anything needs to happen here
                  case _ =>
                }
            }
            dataModel.setMaxTimestamp(vcd.valuesAtTime.keys.max)
          case _ =>
        }
      case None =>
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
