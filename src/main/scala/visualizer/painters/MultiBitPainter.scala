package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.DrawMetrics
import visualizer.controllers.{DecFormat, SelectionController, WaveFormController}
import visualizer.models._

import scala.swing.Graphics2D

class MultiBitPainter(displayModel: WaveFormController) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, top: Int, signal: Signal[_]): Unit = {
    require(signal.waveform.isDefined)
    require(signal.isInstanceOf[PureSignal])

    val pureSignal = signal.asInstanceOf[PureSignal]
    //TODO: pass this format down in here somehow
    val formatter = DecFormat
//    val formatter = waveFormController.waveDisplaySettings(node).dataFormat match {
//      case Some(format) => format
//      case None => DecFormat
//    }
    val startTimestamp = displayModel.xCoordinateToTimestamp(visibleRect.x)
    g.setColor(Color.black)

    // Only paint from first transition at or before the start timestamp
    // up until the first transition after the end timestamp
    try {
      pureSignal.waveform.get.findTransition(startTimestamp).sliding(2).takeWhile { transitionPair =>
        displayModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
      }.foreach { transitionPair =>
        // length could be 1 if findTransition(startTimestamp) has length 1
        if (transitionPair.length == 2) {
          val left: Int = displayModel.timestampToXCoordinate(transitionPair.head.timestamp)
          val right: Int = displayModel.timestampToXCoordinate(transitionPair.last.timestamp)

          g.drawPolygon(Painter.hexagon(left, right, top))

          val labelLeft = math.max(visibleRect.x, left + DrawMetrics.Foo)
          val labelRight = math.min(visibleRect.x + visibleRect.width, right - DrawMetrics.Foo)
          drawLabel(g, labelLeft, labelRight, top, formatter(transitionPair.head.value))
        }
      }
    } catch {
      // If there's only 1 transition in the iterator returned by findTransition,
      // sliding will throw IndexOutOfBoundsException
      case _: IndexOutOfBoundsException =>
    }
  }

  def drawLabel(g: Graphics2D, left: Int, right: Int, top: Int, label: String): Unit = {
    val metrics = g.getFontMetrics(Painter.Arial)
    val fontBaseline = top + (DrawMetrics.WaveformHeight + metrics.getHeight) / 2
    val visibleWidth = right - left

    def tryToDrawString(str: String): Boolean = {
      val stringWidth = metrics.stringWidth(str)
      if (stringWidth < visibleWidth) {
        val fontX = (visibleWidth - stringWidth) / 2 + left
        g.drawString(str, fontX, fontBaseline)
        true
      } else {
        false
      }
    }

    if (!tryToDrawString(label)) {
      // Label doesn't fit so try to draw ellipsis
      val ellipsis = "\u2026"
      tryToDrawString(ellipsis)
    }
  }
}
