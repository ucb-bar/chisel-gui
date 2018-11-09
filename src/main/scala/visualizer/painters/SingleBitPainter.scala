package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.DrawMetrics
import visualizer.controllers.{SelectionController, WaveFormController}
import visualizer.models._

import scala.swing.Graphics2D

class SingleBitPainter(displayModel: WaveFormController) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, top: Int, untypedWaveform: Waveform[_]): Unit = {
    val waveform = untypedWaveform.asInstanceOf[Waveform[BigInt]]

    val startTimestamp = displayModel.xCoordinateToTimestamp(visibleRect.x)
    g.setColor(Color.black)

    // Only paint from first transition at or before the start timestamp
    // up until the first transition after the end timestamp
    try {
      waveform.findTransition(startTimestamp).sliding(2).takeWhile { transitionPair =>
        displayModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
      }.foreach { transitionPair =>
        // length could be 1 if findTransition(startTimestamp) has length 1
        if (transitionPair.length == 2) {
          val left: Int = displayModel.timestampToXCoordinate(transitionPair.head.timestamp)
          val right: Int = displayModel.timestampToXCoordinate(transitionPair.last.timestamp)
          val z = if (transitionPair.head.value == 0) DrawMetrics.WaveformHeight else 0

          // horizontal portion
          g.drawLine(left, top + z, right, top + z)

          // vertical portion
          if (left != 0) {
            g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
          }
        }
      }
    } catch {
      // If there's only 1 transition in the iterator returned by findTransition,
      // sliding will throw IndexOutOfBoundsException
      case _: IndexOutOfBoundsException =>
    }
  }
}
