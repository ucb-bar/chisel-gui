package visualizer.controllers

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import treadle.executable.Symbol
import visualizer.TreadleController
import visualizer.components.SignalSelector
import visualizer.models._

import scala.collection.mutable
import scala.swing.Publisher

/**
  * Manages the selection of signals to be viewed in the WaveForm Viewer
  */
class SelectionController extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model and Pure Signals
  ///////////////////////////////////////////////////////////////////////////
  val directoryTreeModel: SignalSelectionModel = new SignalSelectionModel

  val signalSelector      = new SignalSelector(this)

  val RootPath:  Tree.Path[SelectionNode] = SelectionNode.RootPath

  def insertUnderSorted(parentPath: Path[SelectionNode], newValue: SelectionNode): Boolean = {
    directoryTreeModel.insertUnderSorted(parentPath, newValue)
  }

  def addSymbol(symbol: Symbol): Unit = {
    directoryTreeModel.addSymbol(symbol)
  }

  def addToWaveFormViewer(node: SelectionNode): Unit = {
    TreadleController.waveFormController.addFromDirectoryToInspected(node, signalSelector)
  }
}
