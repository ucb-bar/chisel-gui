package visualizer.models

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import visualizer.{MaxTimestampChanged, TreadleController, Util}

import scala.swing.Publisher

class DataModel extends Publisher {
  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel.empty[DirectoryNode]
  val RootPath: Tree.Path[DirectoryNode] = Tree.Path.empty[DirectoryNode]

  def insertUnderSorted(parentPath: Path[DirectoryNode], newValue: DirectoryNode): Boolean = {
    val children = directoryTreeModel.getChildrenOf(parentPath)

    def search(low: Int = 0, high: Int = children.length - 1): Int = {
      if (high <= low) {
        if (DirectoryNodeOrdering.compare(newValue, children(low)) > 0) low + 1 else low
      } else {
        val mid = (low + high)/2
        DirectoryNodeOrdering.compare(newValue, children(mid)) match {
          case i if i > 0 => search(mid + 1, high)
          case i if i < 0 => search(low, mid - 1)
          case _ => throw new Exception("Duplicate node cannot be added to the directory tree model")
        }
      }
    }

    if (children.isEmpty) {
      directoryTreeModel.insertUnder(parentPath, newValue, 0)
    } else {
      directoryTreeModel.insertUnder(parentPath, newValue, search())
    }
  }


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

object DirectoryNodeOrdering extends Ordering[DirectoryNode] {
  // Sort order: Submodules, Pure signals that are registers, Mixed Signals, Other pure signals
  def compare(x: DirectoryNode, y: DirectoryNode): Int = {
    (x.signal, y.signal) match {
      case (Some(xsignal), Some(ysignal)) =>
        (xsignal, ysignal) match {
          case (xPureSignal: PureSignal, yPureSignal: PureSignal) =>
            (xPureSignal.isRegister, yPureSignal.isRegister) match {
              case (true, false) => -1
              case (false, true) => 1
              case _ => x.name.toLowerCase compareTo y.name.toLowerCase
            }
          case (xPureSignal: PureSignal, _) => if (xPureSignal.isRegister) -1 else 1
          case (_, yPureSignal: PureSignal) => if (yPureSignal.isRegister) 1 else -1
          case _ => x.name.toLowerCase compareTo y.name.toLowerCase
        }
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (None, None) => x.name.toLowerCase compareTo y.name.toLowerCase
    }
  }
}