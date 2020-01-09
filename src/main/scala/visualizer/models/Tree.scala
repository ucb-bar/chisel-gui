package visualizer.models

import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree.{Tree, TreeModel}

import scala.collection.mutable

// Nodes with no signal are groups if InspectedNode, or modules if DirectoryNode

trait GenericTreeNode {
  def name: String
}

trait SignalTreeNode extends GenericTreeNode {
  def name: String

  def signal: Signal[_]
}

case class WaveFormNode(name: String, signal: Signal[_]) extends SignalTreeNode

case class DirectoryNode(name: String) extends GenericTreeNode

trait AddDirection

case object InsertBefore extends AddDirection

case object InsertInto extends AddDirection

case object InsertAfter extends AddDirection

case object AppendToEnd extends AddDirection

object TreeHelper {
  def viewableDepthFirstIterator(tree: Tree[GenericTreeNode]): Iterator[GenericTreeNode] =
    new Iterator[GenericTreeNode] {
      val treeModel: TreeModel[GenericTreeNode] = tree.model
      var openNodes: Iterator[Path[GenericTreeNode]] = treeModel.roots.map(Path(_)).iterator

      def hasNext: Boolean = openNodes.nonEmpty

      def next(): GenericTreeNode =
        if (openNodes.hasNext) {
          val path = openNodes.next()
          pushChildren(path)
          path.last
        } else throw new NoSuchElementException("No more items")

      def pushChildren(path: Path[GenericTreeNode]): Unit = {
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
