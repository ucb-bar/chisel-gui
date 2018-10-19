// See LICENSE for license details.

package visualizer.models

import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree.{InternalTreeModel, Tree, TreeModel}
import treadle.executable.{Symbol, SymbolTable}

import scala.annotation.tailrec

/**
  * Manage the list of signals that can be monitored
  * These signals may appear under groups, groups may be nested
  * Groups may have sub-groups and leaves
  */
class SignalSelectionModel extends InternalTreeModel[SelectionNode] {

  /**
    * Insert a SelectionNode automatically in the correct sorted spot
    * @param parentPath path to this node
    * @param newValue   the new node
    * @return
    */
  def insertUnderSorted(parentPath: Path[SelectionNode], newValue: SelectionNode): Boolean = {
    val children = getChildrenOf(parentPath)

    @tailrec
    def search(low: Int = 0, high: Int = children.length - 1): Int = {
      if (high <= low) {
        if(newValue > children(low)) low + 1 else low
      } else {
        val mid = (low + high)/2
        newValue.compare(children(mid)) match {
          case i if i > 0 => search(mid + 1, high)
          case i if i < 0 => search(low, mid - 1)
          case _ =>
            throw new Exception(s"Duplicate node $newValue cannot be added to the directory tree model")
        }
      }
    }

    val index = if (children.isEmpty) 0 else search()
    insertUnder(parentPath, newValue, index)
  }

  def addSelectionNode(selectionNode: SelectionSignal, stringPath: Seq[String], sortGroup: Int): Unit = {

    val parentPath = stringPath.foldLeft(SignalSelectionModel.RootPath) { (path, module) =>
      val node = SelectionGroup(module, sortGroup)
      val children = getChildrenOf(path)
      if (!children.contains(node)) {
        insertUnderSorted(path, node)
      }
      path :+ node
    }
    insertUnderSorted(parentPath, selectionNode)
  }

  def addSymbol(symbol: Symbol, directory: String = "", sortGroup: Int = 1000): Unit = {
    val path: Seq[String] = if(directory.nonEmpty) directory.split("""[._]""") else Seq.empty[String]

    addSelectionNode(SelectionSignal(symbol), path, sortGroup)
  }

  def walk(thunk: (Path[SelectionNode], SelectionNode) => Unit): Unit = {

    def innerWalk(path: Path[SelectionNode]): Unit= {
      getChildrenOf(path).toList.foreach { child =>
        thunk(path, child)
        innerWalk(path :+ child)
      }
    }

    innerWalk(SignalSelectionModel.RootPath)
  }

  /**
    * Get the tree path to the given name
    * @param nameToFind name to find
    * @return
    */
  def getPathTo(nameToFind: String): Option[Path[SelectionNode]] = {
    var parentPathOpt: Option[Path[SelectionNode]] = None

    walk { (path, node) =>
      if(node.name == nameToFind) {
        parentPathOpt = Some(path :+ node)
      }
    }

    parentPathOpt
  }
}

object SignalSelectionModel {
  val RootPath: Tree.Path[SelectionNode] = Tree.Path.empty[SelectionNode]

  /**
    * Creates and populates a SignalSelectionModel
    * @param symbolTable a treadle symbol table
    * @return
    */
  def apply(symbolTable: SymbolTable): SignalSelectionModel = {
    val signalSelectionModel = new SignalSelectionModel

    symbolTable.symbols.foreach { symbol =>
      if(symbolTable.inputPortsNames.contains(symbol.name)) {
        signalSelectionModel.addSymbol(symbol, "TopLevelInputs", sortGroup = 0)
      }
      else if(symbolTable.outputPortsNames.contains(symbol.name)) {
        signalSelectionModel.addSymbol(symbol, "TopLevelOutputs", sortGroup = 1)
      }
      else {
        val directory = symbol.name.split("""\.""").init.mkString(".")
        signalSelectionModel.addSymbol(symbol, directory)
      }
    }
    signalSelectionModel
  }
}

/**
  * Underlying elements of the SignalSelection Panel
  * elements can be either SelectionGroups (Directories)
  * or SelectionSignals (treadle symbol)
  */
trait SelectionNode extends Ordered[SelectionNode] {
  def name: String
  def sortGroup: Int

  override def compare(that: SelectionNode): Int = {
    if(this.sortGroup == that.sortGroup) {
      if(this.name.toLowerCase < that.name.toLowerCase) {
        -1
      }
      else if(this.name.toLowerCase > that.name.toLowerCase) {
        1
      }
      else {
        0
      }
    }
    else {
      this.sortGroup - that.sortGroup
    }
  }
}

/**
  * A directory of sub-directories and or signals
  * @param name      the directory name
  * @param sortGroup aggregate by sortGroup then alphabetically by name, case insensitive
  */
case class SelectionGroup(name: String, sortGroup: Int = 0) extends SelectionNode

/**
  * A symbol for the selection panel
  * @param symbol    the symbol this entry refers to
  * @param sortGroup aggregate by sortGroup then alphabetically by name, case insensitive
  */
case class SelectionSignal(symbol: Symbol, sortGroup: Int = 1000) extends SelectionNode {
  val name: String = symbol.name
}


