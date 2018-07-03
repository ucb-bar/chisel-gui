package visualizer.components

import java.awt.{FontMetrics, Polygon}

import visualizer.models._

import scala.swing._
import scala.swing.event._

class WaveComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel)
  extends BorderPanel {
  val Foo = 5
  val WaveformHeight = 20
  val WaveformVerticalSpacing = 10
  val Arial = new Font("Arial", 0, 12)

  //
  // View
  //
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect


    // Drawing the waves
    displayModel.displayTreeModel.depthFirstIterator.zipWithIndex.foreach { case (node, row) =>
      val y = row * (WaveformHeight + WaveformVerticalSpacing)

      if (node.id >= 0) {
        dataModel.waveforms(node.id).transitions.sliding(2).foreach { transitionPair =>
          val x0: Int = timeToXCoord(transitionPair(0).timestamp)
          val x1: Int = timeToXCoord(transitionPair(1).timestamp)

          g.drawPolygon(new Polygon(Array(x0, x0+Foo, x1-Foo, x1, x1-Foo, x0+Foo),
            Array(y + WaveformHeight / 2, y, y, y + WaveformHeight / 2, y + WaveformHeight, y + WaveformHeight),
            6)
          )

          drawStringCentered(g, transitionPair(0).value.toString,
            new Rectangle(x0, y, x1 - x0, WaveformHeight),
            Arial)
        }
      } else { // it's a group!
        // do nothing i think?
      }
    }


    // Draw markers
    drawMarkers(g, visibleRect)

    // Draw cursor
    val cursorX = timestampToXCoordinate(displayModel.cursorPosition)
    println(s"(${cursorX}, ${visibleRect.y}, ${cursorX}, ${visibleRect.y + visibleRect.height})")
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

    var startIndex = displayModel.getMarkerAtTime(startTime)
    var endIndex = displayModel.getMarkerAtTime(endTime)

    displayModel.markers.zipWithIndex.foreach{ case (marker, index) =>
      println(s"${index}: ${marker.id}, ${marker.timestamp}")
    }

    println(s"start,end time = $startTime, $endTime")
    println(s"start,end index = $startIndex, $endIndex")

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

  def computeBounds: Unit = {
    preferredSize = new Dimension(timeToXCoord(dataModel.maxTimestamp),
      displayModel.inspectedWaves.size * (WaveformHeight + WaveformVerticalSpacing))
    revalidate()
  }

  //
  // Controller
  //

  listenTo(displayModel)
  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case e @ (_:SignalsAdded | _:ScaleChanged) => wavesAdded
    case e: CursorSet => {
      computeBounds
      repaint
    }
    case e: MarkerChanged => {
      if (e.timestamp < 0) repaint
      else
        repaint(new Rectangle(timestampToXCoordinate(e.timestamp) - 1, 0, 2, peer.getVisibleRect.height))
    }
    case e: MousePressed => {
      val timestamp = xCoordinateToTimestamp(e.peer.getX)
      if (displayModel.cursorPosition != displayModel.selectionStart)
        repaint
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

  def wavesAdded: Unit = {
    computeBounds
    repaint
  }

  def xCoordinateToTimestamp(coordinate: Int): Long = { (coordinate / displayModel.scale).toLong }
  def timestampToXCoordinate(timestamp: Long): Int = { (timestamp * displayModel.scale).toInt }
}
