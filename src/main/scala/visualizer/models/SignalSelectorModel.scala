package visualizer.models

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import visualizer.TreadleController

import scala.annotation.tailrec
import scala.swing.Publisher
import scala.util.matching.Regex

/** This manages the signal available for selection.
  * The list may be filtered.
  *
  */
class SignalSelectorModel extends Publisher {

  ///////////////////////////////////////////////////////////////////////////
  // Directory Tree Model and Pure Signals
  ///////////////////////////////////////////////////////////////////////////
  var directoryTreeModel: InternalTreeModel[GenericTreeNode] = InternalTreeModel.empty[GenericTreeNode]
  val RootPath: Tree.Path[GenericTreeNode] = Tree.Path.empty[GenericTreeNode]

  var dataModelFilter: SelectionModelFilter = SelectionModelFilter()

  def insertUnderSorted(parentPath: Path[GenericTreeNode], newValue: GenericTreeNode): Boolean = {
    val children = try {
      directoryTreeModel.getChildrenOf(parentPath)
    } catch {
      case t: Throwable =>
        println(s"got a problem inserting $newValue into $parentPath")
        throw t
    }
    var dup = false

    @tailrec def search(low: Int = 0, high: Int = children.length - 1): Int = {
      if (high <= low) {
        if (DirectoryNodeOrdering.compare(newValue, children(low)) > 0) low + 1 else low
      } else {
        val mid = (low + high) / 2
        DirectoryNodeOrdering.compare(newValue, children(mid)) match {
          case i if i > 0 => search(mid + 1, high)
          case i if i < 0 => search(low, mid - 1)
          case _ =>
            //            throw new Exception("Duplicate node cannot be added to the directory tree model")
            dup = true
            0
        }
      }
    }

    val index = if (children.isEmpty) 0 else search()
    if (!dup) directoryTreeModel.insertUnder(parentPath, newValue, index)
    !dup
  }

  def addSignalToSelectionList(fullName: String, signal: Signal[_ <: Any]): Unit = {
    // the full name of the signal (from treadle) uses periods to separate modules
    val fullPath = fullName.split("\\.")
    val signalName = fullPath.last
    val modules = fullPath.init

    var lastNodeOpt: Option[DirectoryNode] = None
    if (dataModelFilter.allow(fullPath)) {
      val parentPath = modules.foldLeft(RootPath) { (pathAccumulator, module) =>
        val node = DirectoryNode(module, pathAccumulator)
        val children = directoryTreeModel.getChildrenOf(pathAccumulator)
        if (!children.contains(node)) {
          lastNodeOpt.foreach { lastNode => lastNode.children += node }
          insertUnderSorted(pathAccumulator, node)
        }
        lastNodeOpt = Some(node)
        pathAccumulator :+ node
      }
      val node = WaveFormNode(signalName, signal)
      lastNodeOpt.foreach { lastNode: DirectoryNode =>
        lastNode.children += node
      }
      insertUnderSorted(parentPath, node)
    }
  }

  def updateTreeModel(): Unit = {
    directoryTreeModel = InternalTreeModel.empty[GenericTreeNode]

    TreadleController.dataModel.nameToSignal.foreach {
      case (name, signal) =>
        addSignalToSelectionList(name, signal)
    }
  }
}

object DirectoryNodeOrdering extends Ordering[GenericTreeNode] {
  // Sort order: Submodules, Pure signals that are registers, Mixed Signals, Other pure signals
  def compare(x: GenericTreeNode, y: GenericTreeNode): Int = {
    //TODO: re-institutes coherent sort
    //    (x, y) match {
    //      case (a:    DirectoryNode, b: DirectoryNode) => a.name.toLowerCase.compareTo(b.name.toLowerCase)
    //      case (_, _: DirectoryNode) => 1
    //      case (_: DirectoryNode, b) => -1
    //      case (a: SignalTreeNode, b: SignalTreeNode) =>
    //        (a.signal, b.signal) match {
    //          case (i:    PureSignal, j: PureSignal) => i.sortGroup - j.sortGroup
    //          case (_:    PureSignal, __) => 1
    //          case (_, _: PureSignal) => -1
    //          case (_, _) => a.name.toLowerCase.compareTo(b.name.toLowerCase)
    //        }
    //      case _ => x.name.toLowerCase.compareTo(y.name.toLowerCase)
    //    }
    if (x.name.toLowerCase.compareTo(y.name.toLowerCase) == 0) {
      println(s"dup $x $y")
    }
    x.name.toLowerCase.compareTo(y.name.toLowerCase)
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
                               ) {
  val patternRegex: Regex = pattern.r

  def allow(s: String): Boolean = {
    val isAllowed = {
      patternRegex.findFirstIn(s).isDefined &&
        (!(s.endsWith("_T") || s.contains("_T_")) || showTempVariables) &&
        (!(s.endsWith("_GEN") || s.contains("_GEN_")) || showGenVariables)
    }
    isAllowed
  }

  def allow(sList: Seq[String]): Boolean = {
    sList.forall(allow)
  }
}