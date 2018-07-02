package visualizer.models

import java.awt.datatransfer.{DataFlavor, Transferable, UnsupportedFlavorException}

import javax.swing._
import javax.swing.TransferHandler.TransferSupport
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel}
import scalaswingcontrib.tree._

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scala.swing.event.ActionEvent

class InspectionDisplayModel extends Publisher {
  // TODO: change to tree rather than arraybuffer
  val inspectedWaves = new ArrayBuffer[Waveform]()

  // Replacing inspectedWaves with a TREE
  val temporaryNode = TreeNode("temp", -2)
  val displayTreeModel: InternalTreeModel[TreeNode] = InternalTreeModel(temporaryNode)(_ => Seq.empty[TreeNode])
  val RootPath = Tree.Path.empty[TreeNode]
  val tree = new Tree[TreeNode] {
    model = displayTreeModel
    renderer = Tree.Renderer(_.name) // TODO: use custom renderer to adjust height of row and include value at cursor
    showsRootHandles = true


    // Trying to make it rearrangeable
    peer.setDragEnabled(true)
    peer.setDropMode(DropMode.ON_OR_INSERT)
    peer.setTransferHandler(new TreeTransferHandler)
  }







  var scale: Double = 2
  var minorTickInterval: Long = 0
  val MinMinorTickHSpace = 5

  // initial/constructor
  setScale(10)


  def addSignal(node: TreeNode, source: Component) = {
    displayTreeModel.insertUnder(RootPath, node, displayTreeModel.getChildrenOf(RootPath).size)
    publish(SignalsAdded(source))
  }

  def addModule(moduleNode: TreeNode, source: Component): Unit = {
    displayTreeModel.insertUnder(RootPath, moduleNode, displayTreeModel.getChildrenOf(RootPath).size)

    publish(SignalsAdded(source))
  }

  def setScale(newScale: Double): Unit = {
    scale = newScale
    val x = math.pow(10, math.ceil(math.log10(MinMinorTickHSpace / scale))).toLong
    minorTickInterval = if (x <= 0) 1 else x
  }

  def zoomIn(source: Component): Unit = {
    setScale(scale * 1.25)
    publish(ScaleChanged(source))
  }

  def zoomOut(source: Component): Unit = {
    setScale(scale * 0.8)
    publish(ScaleChanged(source))
  }

}

object InspectionDisplayModel {
  trait Listener {
    def wavesAdded
  }
}

case class Marker(id: Int, description: String, timestamp: Long)

//
// Events
//
case class SignalsAdded(override val source: Component) extends ActionEvent(source)
case class ScaleChanged(override val source: Component) extends ActionEvent(source)

//
// Rearrangeable tree
//
class TreeTransferHandler extends TransferHandler {
  val mimeType: String = DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" +
    classOf[Array[DefaultMutableTreeNode]].toString.drop(6) + "\""
  val nodesFlavor: DataFlavor = new DataFlavor(mimeType)
  val flavors: Array[DataFlavor] = Array[DataFlavor](nodesFlavor)
  var nodesToRemove = Array[DefaultMutableTreeNode]()



  override def canImport(support: TransferSupport): Boolean = {
    val res = support.isDrop && {
      support.setShowDropLocation(true)
      val dropLocation = support.getDropLocation.asInstanceOf[JTree.DropLocation]
      val tree = support.getComponent.asInstanceOf[JTree]
      val dropRow = tree.getRowForPath(dropLocation.getPath)
      val selRows = tree.getSelectionRows
      !selRows.contains(dropRow) && {
        (support.getDropAction != TransferHandler.MOVE || hasCompleteNode(tree)) && {
          val dest = dropLocation.getPath
          val target = dest.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          val path = tree.getPathForRow(selRows(0))
          val firstNode = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          firstNode.getChildCount == 0 || target.getLevel >= firstNode.getLevel
        }
      }
    }
    res
  }

  private def hasCompleteNode(tree: JTree): Boolean = {
    val selRows = tree.getSelectionRows
    var path = tree.getPathForRow(selRows(0))
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

  override def createTransferable(c: JComponent): Transferable = {
    val tree = c.asInstanceOf[JTree]
    val paths = tree.getSelectionPaths
    if (paths != null) {
      val copies = ArrayBuffer[DefaultMutableTreeNode]()
      val toRemove = ArrayBuffer[DefaultMutableTreeNode]()
      var node = paths(0).getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
      copies.append(copy(node))
      toRemove.append(node)
      paths.tail.foreach { path =>
        val next = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
        if (next.getLevel > node.getLevel) {
          copies.append(copy(next))
        } else if (next.getLevel == node.getLevel) {
          copies.append(copy(next))
          toRemove.append(next)
        }
      }
      val nodes = copies.toArray
      nodesToRemove = toRemove.toArray
      return new NodesTransferable(nodes)
    }
    null
  }

  def copy(node: DefaultMutableTreeNode): DefaultMutableTreeNode = {
    node.clone().asInstanceOf[DefaultMutableTreeNode]
  }

  override def exportDone(source: JComponent, data: Transferable, action: Int): Unit = {
    if ((action & TransferHandler.MOVE) == TransferHandler.MOVE) {
      val tree = source.asInstanceOf[JTree]
      val model = tree.getModel.asInstanceOf[DefaultTreeModel]
      nodesToRemove.foreach { node =>
        model.removeNodeFromParent(node)
      }
    }
  }

  override def getSourceActions(c: JComponent): Int = {
    TransferHandler.COPY_OR_MOVE
  }


  override def importData(support: TransferSupport): Boolean = {
    canImport(support) && support.isDrop && {
      // Extract transfer data
      val t = support.getTransferable
      val nodes = t.getTransferData(nodesFlavor).asInstanceOf[Array[DefaultMutableTreeNode]]
      val dropLocation = support.getDropLocation.asInstanceOf[JTree.DropLocation]
      val childIndex = dropLocation.getChildIndex
      val dest = dropLocation.getPath
      val parent = dest.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
      val tree = support.getComponent.asInstanceOf[JTree]
      val model = tree.getModel.asInstanceOf[DefaultTreeModel]

      var index = if (childIndex == -1) parent.getChildCount else childIndex
      nodes.foreach {node =>
        model.insertNodeInto(node, parent, index)
        index += 1
      }
      true
    }
  }

  class NodesTransferable(val nodes: Array[DefaultMutableTreeNode]) extends Transferable {

    @throws(classOf[UnsupportedFlavorException])
    override def getTransferData(flavor: DataFlavor): AnyRef = {
      if (!isDataFlavorSupported(flavor)) {
        throw new UnsupportedFlavorException(flavor)
      }
      nodes
    }

    override def getTransferDataFlavors: Array[DataFlavor] = {
      flavors
    }

    override def isDataFlavorSupported(flavor: DataFlavor) = {
      nodesFlavor.equals(flavor)
    }
  }

}