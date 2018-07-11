package visualizer.painters

import java.awt.Rectangle

import visualizer.DrawMetrics
import visualizer.models.{InspectionDisplayModel, Waveform}

import scala.swing.Graphics2D

class SingleBitPainter(displayModel: InspectionDisplayModel) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, waveform: Waveform, y: Int): Unit = {
    val startTimestamp = xCoordinateToTimestamp(visibleRect.x)

    // Only paint from first transition at or before the start timestamp
    // up until the first transition after the end timestamp
    waveform.findTransition(startTimestamp).sliding(2).takeWhile { transitionPair =>
      timestampToXCoordinate(transitionPair(0).timestamp) < visibleRect.x + visibleRect.width
    }.foreach { transitionPair =>
      val x0: Int = timestampToXCoordinate(transitionPair(0).timestamp)
      val x1: Int = timestampToXCoordinate(transitionPair(1).timestamp)

      val z = if (transitionPair(0).value == 0) DrawMetrics.WaveformHeight else 0

      g.drawLine(x0, y + z, x1, y + z)

      if (x0 != 0) {
        g.drawLine(x0, y, x0, y + DrawMetrics.WaveformHeight)
      }
    }
  }
}
