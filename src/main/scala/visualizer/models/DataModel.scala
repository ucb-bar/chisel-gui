package visualizer.models

import scalaswingcontrib.tree._
import visualizer.{MaxTimestampChanged, TreadleController, Util}

import scala.swing.Publisher

class DataModel extends Publisher {
  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel.empty[DirectoryNode]
  val RootPath: Tree.Path[DirectoryNode] = Tree.Path.empty[DirectoryNode]

  var maxTimestamp: Long = 0
  def updateMaxTimestamp(): Unit = {
    var newMaxTimestamp: Long = 0
    directoryTreeModel.depthFirstIterator.foreach { node =>
      node.signal match {
        case Some(signal) => newMaxTimestamp = math.max(newMaxTimestamp, signal.waveform.last.timestamp)
        case None =>
      }
    }
    if (newMaxTimestamp > maxTimestamp) {
      maxTimestamp = newMaxTimestamp
      publish(new MaxTimestampChanged)
    }
  }

  var timescale: Int = -9

  def loadMoreValues(): Unit = {
    TreadleController.tester match {
      case Some(t) =>
        val clk = t.clockInfoList.head
        val wv = t.waveformValues(startCycle = ((maxTimestamp - clk.initialOffset) / clk.period + 1).toInt)
        Util.toValueChange(wv, initializing = false).foreach {
          case (name, waveform) =>
            assert(TreadleController.pureSignalMapping.contains(name))
            assert(TreadleController.pureSignalMapping(name).signal.isDefined)
            val pureSignal = TreadleController.pureSignalMapping(name).signal.get.asInstanceOf[PureSignal]
            pureSignal.addNewValues(waveform)
        }
         updateMaxTimestamp()
      case None =>
    }
  }
}