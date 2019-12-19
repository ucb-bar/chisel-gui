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

case class DirectoryNode(name: String, signal: Option[Signal[_ <: Any]]) {
  def toInspected: InspectedNode = {
    InspectedNode(name, signal)
  }
}

object TreeHelper {
  def viewableDepthFirstIterator(tree: Tree[InspectedNode]): Iterator[InspectedNode] = new Iterator[InspectedNode] {
    val treeModel: TreeModel[InspectedNode] = tree.model
    var openNodes: Iterator[Path[InspectedNode]] = treeModel.roots.map(Path(_)).iterator

    def hasNext: Boolean = openNodes.nonEmpty
    def next(): InspectedNode =
      if (openNodes.hasNext) {
        val path = openNodes.next()
        pushChildren(path)
        path.last
      } else throw new NoSuchElementException("No more items")

    def pushChildren(path: Path[InspectedNode]): Unit = {
      if (tree.isExpanded(path)) {
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
