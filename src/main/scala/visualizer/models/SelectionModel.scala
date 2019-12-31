package visualizer.models

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import visualizer.TreadleController
import visualizer.TreadleController.dataModel

import scala.annotation.tailrec
import scala.swing.Publisher

/** This manages the signal available for selection.
  * The list may be filtered.
  *
  */
class SelectionModel extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model and Pure Signals
  ///////////////////////////////////////////////////////////////////////////
  var directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel.empty[DirectoryNode]
  val RootPath: Tree.Path[DirectoryNode] = Tree.Path.empty[DirectoryNode]

  var dataModelFilter = SelectionModelFilter()

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
          case _ => throw new Exception("Duplicate node cannot be added to the directory tree model")
        }
      }
    }

    val index = if (children.isEmpty) 0 else search()
    directoryTreeModel.insertUnder(parentPath, newValue, index)
  }

  def addSignalToSelectionList(fullName: String, signal: Signal[_ <: Any]): Unit = {
    // the full name of the signal (from treadle) uses periods to separate modules
    val fullPath = fullName.split("\\.")
    val signalName = fullPath.last
    val modules = fullPath.init

    if (fullName.matches(dataModelFilter.pattern)) {
      if (!(signalName.endsWith("_T") || signalName.contains("_T_")) || dataModelFilter.showTempVariables) {
        if (!(signalName.endsWith("_GEN") || signalName.contains("_GEN_")) || dataModelFilter.showGenVariables) {
          val parentPath = modules.foldLeft(RootPath) { (parentPath, module) =>
            val node = DirectoryNode(module, None)
            val children = directoryTreeModel.getChildrenOf(parentPath)
            if (!children.contains(node)) {
              insertUnderSorted(parentPath, node)
            }
            parentPath :+ node
          }
          val node = DirectoryNode(signalName, Some(signal))
          insertUnderSorted(parentPath, node)
        }
      }
    }
  }

  def updateTreeModel(): Unit = {
    directoryTreeModel = InternalTreeModel.empty[DirectoryNode]

    TreadleController.dataModel.pureSignalMapping.foreach { case (name, signal) =>
      addSignalToSelectionList(name, signal)
    }
    TreadleController.dataModel.combinedSignal.foreach { case (name, signal) =>
      addSignalToSelectionList(name, signal)
    }
  }
}

object DirectoryNodeOrdering extends Ordering[DirectoryNode] {
  // Sort order: Submodules, Pure signals that are registers, Mixed Signals, Other pure signals
  def compare(x: DirectoryNode, y: DirectoryNode): Int = {
    (x.signal, y.signal) match {
      case (Some(xSignal), Some(ySignal)) =>
        (xSignal, ySignal) match {
          case (xPureSignal: PureSignal, yPureSignal: PureSignal) =>
            if (xPureSignal.sortGroup == yPureSignal.sortGroup) {
              x.name.toLowerCase.compareTo(y.name.toLowerCase)
            } else {
              xPureSignal.sortGroup - yPureSignal.sortGroup
            }
          case (xPureSignal: PureSignal, _) => if (xPureSignal.sortGroup <= 1) -1 else 1
          case (_, yPureSignal: PureSignal) => if (yPureSignal.sortGroup <= 1) 1 else -1
          case _ => x.name.toLowerCase.compareTo(y.name.toLowerCase)
        }
      case (None, Some(_)) => -1
      case (Some(_), None) => 1
      case (None, None) => x.name.toLowerCase.compareTo(y.name.toLowerCase)
    }
  }
}

/** Used to control what shows up in the signal selection
  *
  * @param showTempVariables show _T temp wires
  * @param showGenVariables  show _GEN generated wires
  * @param showOnlyRegisters only show registers
  * @param pattern           add a search pattern
  */
case class SelectionModelFilter(
                                 showTempVariables: Boolean = false,
                                 showGenVariables: Boolean = false,
                                 showOnlyRegisters: Boolean = false,
                                 pattern: String = ".*"
                               )
