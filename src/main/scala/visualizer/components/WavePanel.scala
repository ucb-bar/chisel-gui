package visualizer.components

import java.awt.{Color, Rectangle}

import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.models._
import visualizer.painters.{MultiBitPainter, ReadyValidPainter, SingleBitPainter}

import scala.swing._
import scala.swing.event._

class WavePanel(dataModel: DataModel, displayModel: DisplayModel, tree: Tree[GenericTreeNode]) extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  private val multiBitPainter = new MultiBitPainter(displayModel)
  private val singleBitPainter = new SingleBitPainter(displayModel)
  private val readyValidPainter = new ReadyValidPainter(displayModel)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect

    // Set background color
    background = Color.white

    // Draw waveforms
    TreeHelper.viewableDepthFirstIterator(tree).zipWithIndex.foreach {
      case (node: DirectoryNode, row) =>
      case (node: SignalTreeNode, row) =>
        val y = row * DrawMetrics.WaveformVerticalSpacing + DrawMetrics.WaveformVerticalGap
        node.signal match {
          case signal: PureSignal =>
            val isBinary = signal.symbolOpt match {
              case Some(symbol) =>
                symbol.bitWidth == 1
              case None =>
                signal.waveform.get.isBinary
            }
            displayModel.waveDisplaySettings(node.name).painter match {
              case _ =>
                if (isBinary)
                  singleBitPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
                else
                  multiBitPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
            }
          case _: CombinedSignal =>
            readyValidPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
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
    // displayModel.adjustingCursor = true
    case _: MouseReleased => // displayModel.adjustingCursor = false
    case e: MouseDragged =>
      val timestamp = displayModel.xCoordinateToTimestamp(e.peer.getX)
      displayModel.setCursorPosition(timestamp)
      peer.scrollRectToVisible(new Rectangle(e.peer.getX, e.peer.getY, 1, 1))
  }
}
