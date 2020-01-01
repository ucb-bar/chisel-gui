package visualizer.models

import visualizer.TreadleController.dataModel
import visualizer.{MaxTimestampChanged, TreadleController, Util}

import scala.collection.mutable
import scala.swing.Publisher

/** This is the general model for all data relating to signals and their
  * data values. Ground truth resides in the VCD in TreadleController
  *
  */
class DataModel extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model and Pure Signals
  ///////////////////////////////////////////////////////////////////////////
  val pureSignalMapping = new mutable.HashMap[String, PureSignal]
  val combinedSignal = new mutable.HashMap[String, CombinedSignal]

  def ioSignals: Seq[String] = {
    val a = pureSignalMapping.flatMap {
      case (fullName, pureSignal) =>
        if (pureSignal.sortGroup == 0) Some(fullName) else None
    }
    a.toSeq.sorted
  }

  def addSignal(fullName: String, signal: Signal[_ <: Any]): Unit = {
    signal match {
      case pureSignal: PureSignal => dataModel.pureSignalMapping(fullName) = pureSignal
      case combinedSignal: CombinedSignal => dataModel.combinedSignal(fullName) = combinedSignal
      case _ =>
    }
  }

  /** Call this if the vcd has changed in some way
    *
    */
  def loadMoreWaveformValues(): Unit = {
    TreadleController.testerOpt match {
      case Some(t) =>
        t.engine.vcdOption match {
          case Some(vcd) =>
            Util.vcdToTransitions(vcd, initializing = true).foreach {
              case (fullName, transitions) =>
                if (pureSignalMapping.contains(fullName)) {
                  pureSignalMapping(fullName).addNewValues(transitions)
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
  var maxTimestamp: Long = 0

  def setMaxTimestamp(value: Long): Unit = {
    if (value > maxTimestamp) {
      maxTimestamp = value
      publish(new MaxTimestampChanged)
    }
  }

  var timescale: Int = -9
}
