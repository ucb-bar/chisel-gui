package visualizer.components

import java.awt.{Color, Rectangle}

import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.controllers.{SelectionController, WaveFormController}
import visualizer.models._
import visualizer.painters.{MultiBitPainter, ReadyValidPainter, SingleBitPainter}

import scala.swing._
import scala.swing.event._

class WaveComponent(dataModel: SelectionController, displayModel: WaveFormController, tree: Tree[InspectedNode])
  extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  private val multiBitPainter = new MultiBitPainter(dataModel, displayModel)
  private val singleBitPainter = new SingleBitPainter(dataModel, displayModel)
  private val readyValidPainter = new ReadyValidPainter(dataModel, displayModel)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect

    // Set background color
    background = Color.white

    // Draw waveforms
    TreeHelper.viewableDepthFirstIterator(tree).zipWithIndex.foreach { case (node, row) =>
      val y = row * DrawMetrics.WaveformVerticalSpacing + DrawMetrics.WaveformVerticalGap
      node.signal match {
        case Some(signal) if signal.waveform.isDefined =>
          signal match {
            case signal: PureSignal =>
              displayModel.waveDisplaySettings(node.nodeId).painter match {
                case _ =>
                  if (signal.waveform.get.isBinary)
                    singleBitPainter.paintWaveform(g, visibleRect, y, node)
                  else
                    multiBitPainter.paintWaveform(g, visibleRect, y, node)
              }
            case _: CombinedSignal =>
              readyValidPainter.paintWaveform(g, visibleRect, y, node)
          }
        case _ =>
          // node is a group. do nothing?
          // or node doesn't have a waveform
      }
    }

    // Draw markers
    drawMarkers(g, visibleRect)

    // Draw cursor
    g.setColor(new Color(39, 223, 85))
    val cursorX = displayModel.timestampToXCoordinate(displayModel.cursorPosition)
    g.drawLine(cursorX, visibleRect.y, cursorX, visibleRect.y + visibleRect.height)
  }

  def drawMarkers(g: Graphics2D, visibleRect: Rectangle): Unit = {
    val startTime = displayModel.xCoordinateToTimestamp(visibleRect.x)
    val endTime = displayModel.xCoordinateToTimestamp(visibleRect.x + visibleRect.width)
    val startIndex = displayModel.getMarkerAtTime(startTime)
    val endIndex = displayModel.getMarkerAtTime(endTime)

    g.setColor(Color.black)
    displayModel.markers.slice(startIndex, endIndex + 1).foreach { marker =>
      val x = displayModel.timestampToXCoordinate(marker.timestamp)
      g.drawLine(x, 0, x, visibleRect.y + visibleRect.height)
    }
  }

  //
  // Helper functions
  //
  def computeBounds(): Unit = {
    preferredSize = new Dimension(displayModel.timestampToXCoordinate(dataModel.maxTimestamp),
      TreeHelper.viewableDepthFirstIterator(tree).size
        * DrawMetrics.WaveformVerticalSpacing)
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(dataModel, displayModel, tree)
  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case _: SignalsChanged | _: ScaleChanged | _: CursorSet | _: MaxTimestampChanged =>
      computeBounds()
      repaint()
    case _: WaveFormatChanged =>
      repaint()
    case e: MarkerChanged =>
      if (e.timestamp < 0)
        repaint()
      else
        repaint(new Rectangle(displayModel.timestampToXCoordinate(e.timestamp) - 1, 0, 2, peer.getVisibleRect.height))
    case e: MousePressed =>
      val timestamp = displayModel.xCoordinateToTimestamp(e.peer.getX)
      if (displayModel.cursorPosition != displayModel.selectionStart)
        repaint()
      if (!e.peer.isShiftDown)
        displayModel.selectionStart = timestamp
      displayModel.setCursorPosition(timestamp)
      // waveFormController.adjustingCursor = true
    case _: MouseReleased => // waveFormController.adjustingCursor = false
    case e: MouseDragged =>
      val timestamp = displayModel.xCoordinateToTimestamp(e.peer.getX)
      displayModel.setCursorPosition(timestamp)
      peer.scrollRectToVisible(new Rectangle(e.peer.getX, e.peer.getY, 1, 1))
  }
}