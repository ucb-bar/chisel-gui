package visualizer.models

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import visualizer.{MaxTimestampChanged, TreadleController, Util}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.swing.Publisher

/** This is the general model for all data relating to signals and their
  * data values
  *
  */
class DataModel extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model and Pure Signals
  ///////////////////////////////////////////////////////////////////////////
  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel.empty[DirectoryNode]
  val RootPath:           Tree.Path[DirectoryNode] = Tree.Path.empty[DirectoryNode]
  val pureSignalMapping = new mutable.HashMap[String, PureSignal]

  def insertUnderSorted(parentPath: Path[DirectoryNode], newValue: DirectoryNode): Boolean = {
    val children = directoryTreeModel.getChildrenOf(parentPath)

    @tailrec def search(low: Int = 0, high: Int = children.length - 1): Int = {
      if (high <= low) {
        if (DirectoryNodeOrdering.compare(newValue, children(low)) > 0) low + 1 else low
      } else {
        val mid = (low + high) / 2
        DirectoryNodeOrdering.compare(newValue, children(mid)) match {
          case i if i > 0 => search(mid + 1, high)
          case i if i < 0 => search(low, mid - 1)
          case _          => throw new Exception("Duplicate node cannot be added to the directory tree model")
        }
      }
    }

    val index = if (children.isEmpty) 0 else search()
    directoryTreeModel.insertUnder(parentPath, newValue, index)
  }

  def ioSignals: Seq[String] = {
    val a = pureSignalMapping.flatMap {
      case (fullName, pureSignal) =>
        if (pureSignal.sortGroup == 0) Some(fullName) else None
    }
    a.toSeq.sorted
  }

  // TODO: move tester part to TreadleController?
  def loadMoreWaveformValues(): Unit = {
    TreadleController.tester match {
      case Some(t) =>
        val clk = t.clockInfoList.head
        val wv = t.waveformValues(startCycle = ((maxTimestamp - clk.initialOffset) / clk.period + 1).toInt)
        Util.toValueChange(wv, initializing = false).foreach {
          case (fullName, waveform) =>
            if (pureSignalMapping.contains(fullName)) {
              pureSignalMapping(fullName).addNewValues(waveform)
            }
        }
        updateMaxTimestamp()
      case None =>
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Timescale and Max Timestamp
  ///////////////////////////////////////////////////////////////////////////
  var maxTimestamp: Long = 0
  def updateMaxTimestamp(): Unit = {
    var newMaxTimestamp: Long = 0
    directoryTreeModel.depthFirstIterator.foreach { node =>
      node.signal match {
        case Some(signal) if signal.waveform.isDefined =>
          newMaxTimestamp = math.max(newMaxTimestamp, signal.waveform.get.transitions.last.timestamp)
        case _ =>
      }
    }
    if (newMaxTimestamp > maxTimestamp) {
      maxTimestamp = newMaxTimestamp
      publish(new MaxTimestampChanged)
    }
  }
  var timescale: Int = -9
}

object DirectoryNodeOrdering extends Ordering[DirectoryNode] {
  // Sort order: Submodules, Pure signals that are registers, Mixed Signals, Other pure signals
  def compare(x: DirectoryNode, y: DirectoryNode): Int = {
    (x.signal, y.signal) match {
      case (Some(xsignal), Some(ysignal)) =>
        (xsignal, ysignal) match {
          case (xPureSignal: PureSignal, yPureSignal: PureSignal) =>
            if (xPureSignal.sortGroup == yPureSignal.sortGroup) {
              x.name.toLowerCase.compareTo(y.name.toLowerCase)
            } else {
              xPureSignal.sortGroup - yPureSignal.sortGroup
            }
          case (xPureSignal:    PureSignal, _) => if (xPureSignal.sortGroup <= 1) -1 else 1
          case (_, yPureSignal: PureSignal)    => if (yPureSignal.sortGroup <= 1) 1 else -1
          case _ => x.name.toLowerCase.compareTo(y.name.toLowerCase)
        }
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (None, None)    => x.name.toLowerCase.compareTo(y.name.toLowerCase)
    }
  }
}
