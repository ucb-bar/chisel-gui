package visualizer.painters

import java.awt.Rectangle

import visualizer.DrawMetrics
import visualizer.models.{InspectionDataModel, InspectionDisplayModel}

import scala.swing.Graphics2D

class SingleBitPainter(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, signalId: Int, top: Int): Unit = {
    val waveform = dataModel.waveforms(signalId)
    val startTimestamp = xCoordinateToTimestamp(visibleRect.x)

    // Only paint from first transition at or before the start timestamp
    // up until the first transition after the end timestamp
    waveform.findTransition(startTimestamp).sliding(2).takeWhile { transitionPair =>
      timestampToXCoordinate(transitionPair(0).timestamp) < visibleRect.x + visibleRect.width
    }.foreach { transitionPair =>
      val x0: Int = timestampToXCoordinate(transitionPair(0).timestamp)
      val x1: Int = timestampToXCoordinate(transitionPair(1).timestamp)

      val z = if (transitionPair(0).value == 0) DrawMetrics.WaveformHeight else 0

      g.drawLine(x0, top + z, x1, top + z)

      if (x0 != 0) {
        g.drawLine(x0, top, x0, top + DrawMetrics.WaveformHeight)
      }
    }
  }
}
