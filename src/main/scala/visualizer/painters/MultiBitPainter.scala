package visualizer.painters

import java.awt.{Polygon, Rectangle}

import visualizer.DrawMetrics
import visualizer.models.{DecFormat, InspectionDataModel, InspectionDisplayModel}

import scala.swing.Graphics2D

class MultiBitPainter(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, signalId: Int, top: Int): Unit = {
    val waveform = dataModel.waveforms(signalId)
    val startTimestamp = xCoordinateToTimestamp(visibleRect.x)
    val formatter = displayModel.waveDisplaySettings(signalId).dataFormat match {
      case Some(format) => format
      case None => DecFormat
    }

    // Only paint from first transition at or before the start timestamp
    // up until the first transition after the end timestamp
    waveform.findTransition(startTimestamp).sliding(2).takeWhile { transitionPair =>
      timestampToXCoordinate(transitionPair(0).timestamp) < visibleRect.x + visibleRect.width
    }.foreach { transitionPair =>
      val x0: Int = timestampToXCoordinate(transitionPair(0).timestamp)
      val x1: Int = timestampToXCoordinate(transitionPair(1).timestamp)

      g.drawPolygon(new Polygon(Array(x0, x0 + DrawMetrics.Foo, x1 - DrawMetrics.Foo, x1, x1 - DrawMetrics.Foo, x0 + DrawMetrics.Foo),
        Array(top + DrawMetrics.WaveformHeight / 2, top, top, top + DrawMetrics.WaveformHeight / 2, top + DrawMetrics.WaveformHeight, top + DrawMetrics.WaveformHeight),
        6)
      )

      drawLabel(
        g,
        Math.max(visibleRect.x, x0 + DrawMetrics.Foo),
        Math.min(visibleRect.x + visibleRect.width, x1 - DrawMetrics.Foo),
        top,
        formatter(transitionPair(0).value)
      )
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
