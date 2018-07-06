package visualizer.components

import java.awt.{FontMetrics, Polygon}

import javax.swing.tree.{DefaultMutableTreeNode, TreeNode => JTreeNode}
import scalaswingcontrib.event.{TreeCollapsed, TreeExpanded}
import scalaswingcontrib.tree.TreeModel
import visualizer._
import visualizer.models._

import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.swing._
import scala.swing.event._

class WaveComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel)
  extends BorderPanel {
  val Arial = new Font("Arial", 0, 12)

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect

    // Drawing the waves
    val dfsIterator = enumerationAsScalaIterator(displayModel.displayTreeModel.peer.getRoot.asInstanceOf[DefaultMutableTreeNode].preorderEnumeration)
//    var y = 0
    displayModel.viewableDepthFristIterator().zipWithIndex.foreach { case (node, row) =>
//    dfsIterator.zipWithIndex.foreach { case (jnode, row) =>
      try {
//        val node = jnode.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[TreeNode]
//        print(s"($row, ${node.name}, ${node.nodeId}) ")
        val y = row * (DrawMetrics.WaveformHeight + DrawMetrics.WaveformVerticalSpacing)
        if (node.waveId >= 0) {
          dataModel.waveforms(node.waveId).transitions.sliding(2).foreach { transitionPair =>
            val x0: Int = timeToXCoord(transitionPair(0).timestamp)
            val x1: Int = timeToXCoord(transitionPair(1).timestamp)

            g.drawPolygon(new Polygon(Array(x0, x0 + DrawMetrics.Foo, x1 - DrawMetrics.Foo, x1, x1 - DrawMetrics.Foo, x0 + DrawMetrics.Foo),
              Array(y + DrawMetrics.WaveformHeight / 2, y, y, y + DrawMetrics.WaveformHeight / 2, y + DrawMetrics.WaveformHeight, y + DrawMetrics.WaveformHeight),
              6)
            )

            drawStringCentered(g, transitionPair(0).value.toString,
              new Rectangle(x0, y, x1 - x0, DrawMetrics.WaveformHeight),
              Arial)
          }

        } else { // it's a group!
          // do nothing i think?
        }
//        y += (DrawMetrics.WaveformHeight + DrawMetrics.WaveformVerticalSpacing)
      } catch {
        case _: Throwable => //println(s"row: $row")
      }
    }
//    println()
//    displayModel.displayTreeModel.getChildrenOf(displayModel.RootPath).foreach{node =>
//      print(s"${node.name} ")
//    }
//    println()


    // Draw markers
    drawMarkers(g, visibleRect)

    // Draw cursor
    val cursorX = timestampToXCoordinate(displayModel.cursorPosition)
    g.drawLine(cursorX, visibleRect.y, cursorX, visibleRect.y + visibleRect.height)
  }

  def drawStringCentered(g: Graphics2D, text: String, rect: Rectangle, font: Font): Unit = {
    val metrics: FontMetrics = g.getFontMetrics(font)
    val x = rect.x + (rect.width - metrics.stringWidth(text)) / 2
    val y = rect.y + ((rect.height - metrics.getHeight) / 2) + metrics.getAscent
    g.setFont(font)
    g.drawString(text, x, y)
  }

  def drawMarkers(g: Graphics2D, visibleRect: Rectangle): Unit = {
    val startTime = xCoordinateToTimestamp(visibleRect.x)
    val endTime = xCoordinateToTimestamp(visibleRect.x + visibleRect.width)

    val startIndex = displayModel.getMarkerAtTime(startTime)
    val endIndex = displayModel.getMarkerAtTime(endTime)

    displayModel.markers.slice(startIndex, endIndex + 1).foreach { marker =>
      val x = timestampToXCoordinate(marker.timestamp)
      g.drawLine(x, 0, x, visibleRect.y + visibleRect.height)
    }
  }

  //
  // Helper functions
  //

  def timeToXCoord(timestamp: Long): Int = {
    (timestamp * displayModel.scale).toInt
  }

  def computeBounds(): Unit = {
    preferredSize = new Dimension(timeToXCoord(dataModel.maxTimestamp),
      displayModel.viewableDepthFristIterator().size
        * (DrawMetrics.WaveformHeight + DrawMetrics.WaveformVerticalSpacing))
    println(s"(${preferredSize.width}, ${preferredSize.height})")
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////

  listenTo(displayModel, displayModel.tree)
  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case e @ (_:SignalsChanged | _:ScaleChanged) => {
      computeBounds()
      repaint()
    }
    case e: CursorSet => {
      computeBounds()
      repaint()
    }
    case e @ (_:TreeExpanded[InspectedNode] | _:TreeCollapsed[InspectedNode]) => {
      computeBounds()
      repaint()
    }
    case e: MarkerChanged => {
      if (e.timestamp < 0) repaint()
      else
        repaint(new Rectangle(timestampToXCoordinate(e.timestamp) - 1, 0, 2, peer.getVisibleRect.height))
    }
    case e: MousePressed => {
      val timestamp = xCoordinateToTimestamp(e.peer.getX)
      if (displayModel.cursorPosition != displayModel.selectionStart)
        repaint()
      if (!e.peer.isShiftDown)
        displayModel.selectionStart = timestamp
      displayModel.setCursorPosition(timestamp)
      // displayModel.adjustingCursor = true

    }
    case e: MouseReleased => // displayModel.adjustingCursor = false
    case e: MouseDragged => {
      val timestamp = xCoordinateToTimestamp(e.peer.getX)
      displayModel.setCursorPosition(timestamp)
      peer.scrollRectToVisible(new Rectangle(e.peer.getX, e.peer.getY, 1, 1))
    }
  }

  def xCoordinateToTimestamp(coordinate: Int): Long = { (coordinate / displayModel.scale).toLong }
  def timestampToXCoordinate(timestamp: Long): Int = { (timestamp * displayModel.scale).toInt }
}