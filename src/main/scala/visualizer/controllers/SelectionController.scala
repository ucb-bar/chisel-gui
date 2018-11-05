package visualizer.controllers

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import visualizer.components.SignalSelector
import visualizer.models._
import visualizer.{MaxTimestampChanged, TreadleController, Util}
import treadle.executable.Symbol

import scala.annotation.tailrec
import scala.collection.mutable
import scala.swing.Publisher

/**
  * Manages the selection of signals to be viewed in the WaveForm Viewer
  */
class SelectionController extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model and Pure Signals
  ///////////////////////////////////////////////////////////////////////////
//  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel.empty[DirectoryNode]
  val directoryTreeModel: SignalSelectionModel = new SignalSelectionModel

  val signalSelector      = new SignalSelector(this)

  val RootPath:  Tree.Path[SelectionNode] = SelectionNode.RootPath

  val pureSignalMapping = new mutable.HashMap[String, PureSignal]

  def insertUnderSorted(parentPath: Path[SelectionNode], newValue: SelectionNode): Boolean = {
    val children = directoryTreeModel.getChildrenOf(parentPath)

    @tailrec def search(low: Int = 0, high: Int = children.length - 1): Int = {
      if (high <= low) {
        if (newValue < children(low)) low + 1 else low
      } else {
        val mid = (low + high)/2
        newValue.compare(children(mid)) match {
          case i if i > 0 => search(mid + 1, high)
          case i if i < 0 => search(low, mid - 1)
          case _ => throw new Exception("Duplicate node cannot be added to the directory tree model")
        }
      }
    }

    val index = if (children.isEmpty) 0 else search()
    directoryTreeModel.insertUnder(parentPath, newValue, index)
  }

  def addSymbol(symbol: Symbol): Unit = {
    directoryTreeModel.addSymbol(symbol)
  }

  def addToWaveFormViewer(node: SelectionNode): Unit = {
    TreadleController.waveFormController.addFromDirectoryToInspected(node, signalSelector)
  }

  // TODO: move tester part to TreadleController?
  def loadMoreWaveformValues(): Unit = {
//    TreadleController.tester match {
//      case Some(t) =>
//        val clk = t.clockInfoList.head
//        val wv = t.waveformValues(startCycle = ((maxTimestamp - clk.initialOffset) / clk.period + 1).toInt)
//        Util.toValueChange(wv, initializing = false).foreach {
//          case (fullName, waveform) =>
//            if(pureSignalMapping.contains(fullName)) {
//              pureSignalMapping(fullName).addNewValues(waveform)
//            }
//        }
//        updateMaxTimestamp()
//      case None =>
//    }
  }
}

object DirectoryNodeOrdering extends Ordering[DirectoryNode] {
  // Sort order: Submodules, Pure signals that are registers, Mixed Signals, Other pure signals
  def compare(x: DirectoryNode, y: DirectoryNode): Int = {
    (x.signal, y.signal) match {
      case (Some(xsignal), Some(ysignal)) =>
        (xsignal, ysignal) match {
          case (xPureSignal: PureSignal, yPureSignal: PureSignal) =>
            if (xPureSignal.sortGroup == yPureSignal.sortGroup) {
              x.name.toLowerCase compareTo y.name.toLowerCase
            } else {
              xPureSignal.sortGroup - yPureSignal.sortGroup
            }
          case (xPureSignal: PureSignal, _) => if (xPureSignal.sortGroup <= 1) -1 else 1
          case (_, yPureSignal: PureSignal) => if (yPureSignal.sortGroup <= 1) 1 else -1
          case _ => x.name.toLowerCase compareTo y.name.toLowerCase
        }
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (None, None) => x.name.toLowerCase compareTo y.name.toLowerCase
    }
  }
}