package visualizer.models

import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree.{Tree, TreeModel}

// Nodes with no signal are groups if InspectedNode, or modules if DirectoryNode
class InspectedNode(val nodeId: Int, val name: String, val signal: Option[Signal[_ <: Any]]) {
  def copy: InspectedNode = {
    InspectedNode(name, signal)
  }
}

object InspectedNode {
  private var nodeId = 0

  def apply(name: String, signal: Option[Signal[_ <: Any]]): InspectedNode = {
    nodeId += 1
    new InspectedNode(nodeId, name, signal)
  }
}

object TreeHelper {
  def viewableDepthFirstIterator(tree: Tree[SelectionNode]): Iterator[SelectionNode] = new Iterator[SelectionNode] {
    val treeModel: TreeModel[SelectionNode] = tree.model
    var openNodes: Iterator[Path[SelectionNode]] = treeModel.roots.map(Path(_)).iterator

    def hasNext: Boolean = openNodes.nonEmpty
    def next(): SelectionNode = if (openNodes.hasNext) {
      val path = openNodes.next()
      pushChildren(path)
      path.last
    } else throw new NoSuchElementException("No more items")

    def pushChildren(path: Path[SelectionNode]): Unit = {
      if (tree.isExpanded(path)) {
        val open = openNodes
        openNodes = treeModel.getChildPathsOf(path).toIterator ++ open
      }
    }
  }

  // the boolean argument sets whether children hidden in the UI are pushed onto the stack and iterated over
  // defaults to false, meaning paths to all nodes are explored, even if they are hidden
  def pathDepthFirstIterator(tree: Tree[SelectionNode], expandedOnly: Boolean = false): Iterator[Seq[SelectionNode]] = new Iterator[Seq[SelectionNode]] {
    val treeModel: TreeModel[SelectionNode] = tree.model
    var openNodes: Iterator[Path[SelectionNode]] = treeModel.roots.map(Path(_)).iterator

    def hasNext: Boolean = openNodes.nonEmpty
    def next(): Seq[SelectionNode] = if (openNodes.hasNext) {
      val path = openNodes.next()
      pushChildren(path)
      return path.seq
    } else throw new NoSuchElementException("No more items")

    def pushChildren(path: Path[SelectionNode]): Unit = {
      if (!expandedOnly || tree.isExpanded(path)) {
        val open = openNodes
        openNodes = treeModel.getChildPathsOf(path).toIterator ++ open
      }
    }
  }

  def hasCompleteNode(tree: JTree): Boolean = {
    val selRows = tree.getSelectionRows
    var path = tree.getSelectionPath
    val first = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
    val childCount = first.getChildCount
    (childCount == 0 || selRows.length != 1) && {
      !selRows.tail.exists { row =>
        path = tree.getPathForRow(row)
        val next = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
        first.isNodeChild(next) && (childCount > selRows.length - 1)
      }
    }
  }
}