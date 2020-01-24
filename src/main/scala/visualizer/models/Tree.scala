package visualizer.models

import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree.{Tree, TreeModel}

// Nodes with no signal are groups if InspectedNode, or modules if DirectoryNode

object GenericTreeNode {
  var lastNodeId: Long = 0

  def assignNodeId(): Long = {
    lastNodeId += 1L
    lastNodeId
  }
}

trait GenericTreeNode {
  def nodeId: Long

  def name: String

  def serialize: String = s"${getClass.getName}($name)"
}

trait SignalTreeNode extends GenericTreeNode {
  def signal: Signal
}

case class WaveFormNode(name: String, signal: Signal, nodeId: Long) extends SignalTreeNode {
  override def serialize: String = {
    signal match {
      case p: PureSignal =>
        s"${getClass.getName}(${p.name})"
      case _ =>
        super.serialize
    }
  }
}

object WaveFormNode {
  def apply(name: String, signal: Signal): WaveFormNode = {
    WaveFormNode(name, signal, GenericTreeNode.assignNodeId())
  }
}

case class DirectoryNode(name: String, nodeId: Long) extends GenericTreeNode

object DirectoryNode {
  def apply(name: String): DirectoryNode = {
    new DirectoryNode(name, GenericTreeNode.assignNodeId())
  }
}

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
