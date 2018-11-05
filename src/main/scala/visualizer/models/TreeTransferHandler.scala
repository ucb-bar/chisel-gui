package visualizer.models

import java.awt.datatransfer.{DataFlavor, Transferable, UnsupportedFlavorException}

import javax.swing._
import javax.swing.TransferHandler.TransferSupport
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeModel}
import visualizer.controllers.WaveFormController

import scala.collection.mutable.ArrayBuffer

//
// Transfer handler to make tree rearrangeable
//
class TreeTransferHandler(displayModel: WaveFormController) extends TransferHandler {
  val mimeType: String = DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" +
    classOf[Array[DefaultMutableTreeNode]].toString.drop(6) + "\""
  val nodesFlavor: DataFlavor = new DataFlavor(mimeType)
  val flavors: Array[DataFlavor] = Array[DataFlavor](nodesFlavor)
  var nodesToRemove: Array[DefaultMutableTreeNode] = Array[DefaultMutableTreeNode]()

  override def canImport(support: TransferSupport): Boolean = {
    val res = support.isDrop && {
      support.setShowDropLocation(true)
      val dropLocation = support.getDropLocation.asInstanceOf[JTree.DropLocation]
      val tree = support.getComponent.asInstanceOf[JTree]
      val dropRow = tree.getRowForPath(dropLocation.getPath)
      val selRows = tree.getSelectionRows
      !selRows.contains(dropRow) && {
        (support.getDropAction != TransferHandler.MOVE || TreeHelper.hasCompleteNode(tree)) && {
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

  override def createTransferable(c: JComponent): Transferable = {
    val tree = c.asInstanceOf[JTree]
    val paths = tree.getSelectionPaths
    if (paths == null) {
      null
    } else {
      val copies = ArrayBuffer[DefaultMutableTreeNode]()
      val toRemove = ArrayBuffer[DefaultMutableTreeNode]()
      val node = paths(0).getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
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
      new NodesTransferable(nodes)
    }
  }

  def copy(node: DefaultMutableTreeNode): DefaultMutableTreeNode = {
    val originalSelectionNode = node.getUserObject.asInstanceOf[SelectionNode]
    val copiedSelectionNode = originalSelectionNode
//    originalSelectionNode.signal match {
//      case Some(_) =>
//        displayModel.waveDisplaySettings(copiedSelectionNode.nodeId) =
//          displayModel.waveDisplaySettings(originalSelectionNode.nodeId)
//      case None =>
//    }
    new DefaultMutableTreeNode(copiedSelectionNode)
  }

  override def exportDone(source: JComponent, data: Transferable, action: Int): Unit = {
    if ((action & TransferHandler.MOVE) == TransferHandler.MOVE) {
      val tree = source.asInstanceOf[JTree]
      val model = tree.getModel.asInstanceOf[DefaultTreeModel]
      nodesToRemove.foreach { node =>
        model.removeNodeFromParent(node)
      }
      displayModel.moveSignals(null)
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

      // childIndex is -1 if the selected nodes are being inserted rather than placed within another node
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

    override def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
      nodesFlavor.equals(flavor)
    }
  }

}