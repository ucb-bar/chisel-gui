package visualizer.models

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import visualizer.ChiselGUI

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
  val RootPath:           Tree.Path[GenericTreeNode] = Tree.Path.empty[GenericTreeNode]

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
          case _          =>
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

  def addSignalToSelectionList(fullName: String, signal: Signal): Unit = {
    // the full name of the signal (from treadle) uses periods to separate modules
    val fullPath = fullName.split("\\.")
    val signalName = fullPath.last
    val modules = fullPath.init

    if (dataModelFilter.allow(fullName)) {
      val parentPath = modules.foldLeft(RootPath) { (pathAccumulator, module) =>
        val children = directoryTreeModel.getChildrenOf(pathAccumulator)
        val dirNode = children.find(child => child.name == module).getOrElse {
          val newDirNode = DirectoryNode(module)
          insertUnderSorted(pathAccumulator, newDirNode)
          newDirNode
        }
        pathAccumulator :+ dirNode
      }
      val node = WaveFormNode(signalName, signal)

      insertUnderSorted(parentPath, node)

      node.signal match {
        case decoupledSignalGroup: DecoupledSignalGroup =>
          val decoupledPath = parentPath ++ Seq(node)
          decoupledSignalGroup.bitsSignals.foreach { bitSignal =>
            insertUnderSorted(decoupledPath, WaveFormNode(bitSignal.name, bitSignal))
          }
        case validSignalGroup: ValidSignalGroup =>
          val decoupledPath = parentPath ++ Seq(node)
          validSignalGroup.bitsSignals.foreach { bitSignal =>
            insertUnderSorted(decoupledPath, WaveFormNode(bitSignal.name, bitSignal))
          }
        case _ =>
      }
    }
  }

  def setRollupDecoupled(value: Boolean): Unit = {
    dataModelFilter = dataModelFilter.copy(rollupDecoupled = value)
  }

  /** Add all the names in te data model as a candidate for the
    * select list, names may get filtered in addSignalToSelectionList
    */
  //TODO: Not right if not sorted first, this should get fixed, it ought to work unsorted, which would be quicker
  def updateTreeModel(): Unit = {
    directoryTreeModel = InternalTreeModel.empty[GenericTreeNode]

    ChiselGUI.dataModel.nameToSignal.keys.toSeq.sorted.foreach { name =>
      addSignalToSelectionList(name, ChiselGUI.dataModel.nameToSignal(name))
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
  * @param rollupDecoupled   don't show field as is fields that are part of decoupleds
  * @param hiddenDecoupled   list of fields not to show when rolling up
  * @param pattern           add a search pattern
  */
case class SelectionModelFilter(
  showTempVariables: Boolean = false,
  showGenVariables:  Boolean = false,
  showOnlyRegisters: Boolean = false,
  rollupDecoupled:   Boolean = true,
  hiddenDecoupled:   Seq[String] = Seq.empty,
  pattern:           String = ".*"
) {
  val patternRegex: Regex = pattern.r

  def allow(s: String): Boolean = {
    val isHiddenDecoupledSignal = if (rollupDecoupled) {
      hiddenDecoupled.contains(s)
    } else {
      s.endsWith("Decoupled")
    }

    val isAllowed = {
      patternRegex.findFirstIn(s).isDefined &&
      (!(s.endsWith("_T") || s.contains("_T_")) || showTempVariables) &&
      (!(s.endsWith("_GEN") || s.contains("_GEN_")) || showGenVariables) &&
      (!isHiddenDecoupledSignal)
    }
    isAllowed
  }

  def allow(sList: Seq[String]): Boolean = {
    sList.forall(allow)
  }
}
