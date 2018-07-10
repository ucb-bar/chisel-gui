package visualizer.components

import java.awt.{FontMetrics, Polygon, Rectangle}

import scalaswingcontrib.event.{TreeCollapsed, TreeExpanded}
import visualizer._
import visualizer.models._
import visualizer.painters.{MultiBitPainter, SingleBitPainter}

import scala.swing._
import scala.swing.event._

class WaveComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel)
  extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////

  private val multiBitPainter = new MultiBitPainter(displayModel)
  private val singleBitPainter = new SingleBitPainter(displayModel)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect

    // Drawing the waves
    TreeHelper.viewableDepthFirstIterator(displayModel.tree).zipWithIndex.foreach { case (node, row) =>
      val y = row * (DrawMetrics.WaveformHeight + DrawMetrics.WaveformVerticalSpacing)
      if (node.waveId >= 0) {
        displayModel.waveDisplaySettings(node.waveId).painter match {
          case _ =>
            if (dataModel.waveforms(node.waveId).isBinary)
              singleBitPainter.paintWaveform(g, dataModel.waveforms(node.waveId), y)
            else
              multiBitPainter.paintWaveform(g, dataModel.waveforms(node.waveId), y)
        }
      } else {
        // node is a group. do nothing?
      }
    }

    // Draw markers
    drawMarkers(g, visibleRect)

    // Draw cursor
    val cursorX = timestampToXCoordinate(displayModel.cursorPosition)
    g.drawLine(cursorX, visibleRect.y, cursorX, visibleRect.y + visibleRect.height)
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
  def computeBounds(): Unit = {
    preferredSize = new Dimension(timestampToXCoordinate(dataModel.maxTimestamp),
      TreeHelper.viewableDepthFirstIterator(displayModel.tree).size
        * (DrawMetrics.WaveformHeight + DrawMetrics.WaveformVerticalSpacing))
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////

  listenTo(displayModel, displayModel.tree)
  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case _ @ (_:SignalsChanged | _:ScaleChanged) =>
      computeBounds()
      repaint()
    case _:ScaleChanged =>

    case _: CursorSet =>
      computeBounds()
      repaint()
    case _ @ (_:TreeExpanded[InspectedNode] | _:TreeCollapsed[InspectedNode]) =>
      computeBounds()
      repaint()
    case e: MarkerChanged =>
      if (e.timestamp < 0)
        repaint()
      else
        repaint(new Rectangle(timestampToXCoordinate(e.timestamp) - 1, 0, 2, peer.getVisibleRect.height))
    case e: MousePressed =>
      val timestamp = xCoordinateToTimestamp(e.peer.getX)
      if (displayModel.cursorPosition != displayModel.selectionStart)
        repaint()
      if (!e.peer.isShiftDown)
        displayModel.selectionStart = timestamp
      displayModel.setCursorPosition(timestamp)
      // displayModel.adjustingCursor = true
    case _: MouseReleased => // displayModel.adjustingCursor = false
    case e: MouseDragged =>
      val timestamp = xCoordinateToTimestamp(e.peer.getX)
      displayModel.setCursorPosition(timestamp)
      peer.scrollRectToVisible(new Rectangle(e.peer.getX, e.peer.getY, 1, 1))
  }

  def xCoordinateToTimestamp(coordinate: Int): Long = { (coordinate / displayModel.scale).toLong }
  def timestampToXCoordinate(timestamp: Long): Int = { (timestamp * displayModel.scale).toInt }
}