package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.DrawMetrics
import visualizer.models._

import scala.swing.Graphics2D

class SingleBitPainter(displayModel: DisplayModel) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, top: Int, node: InspectedNode, maxTimestamp: Long): Unit = {
    require(node.signal.isDefined)
    val signal = node.signal.get
    require(signal.waveform.isDefined)
    require(signal.isInstanceOf[PureSignal])

    val pureSignal = signal.asInstanceOf[PureSignal]
    val startTimestamp = displayModel.xCoordinateToTimestamp(visibleRect.x)
    g.setColor(Color.black)

    // Only paint from first transition at or before the start timestamp
    // up until the first transition after the end timestamp
    try {
      pureSignal.waveform.get
        .findTransition(startTimestamp)
        .sliding(2)
        .takeWhile { transitionPair =>
          displayModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
        }
        .foreach {
          case transition1 :: transition2 :: Nil =>
            val left: Int = displayModel.timestampToXCoordinate(transition1.timestamp)
            val right: Int = displayModel.timestampToXCoordinate(transition2.timestamp)
            val z = if (transition1.value == 0) DrawMetrics.WaveformHeight else 0

            // horizontal portion
            g.drawLine(left, top + z, right, top + z)

            // vertical portion
            if (left != 0) {
              g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
            }
          case transition :: Nil =>
            val left: Int = displayModel.timestampToXCoordinate(0L)
            val right: Int = displayModel.timestampToXCoordinate(transition.timestamp)
            val z = if (transition.value == 0) DrawMetrics.WaveformHeight else 0

            // horizontal portion
            g.drawLine(left, top + z, right, top + z)

            // vertical portion
            if (left != 0) {
              g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
            }
        }

      pureSignal.waveform.get.transitions.lastOption match {
        case Some(lastTransition) =>
          if(lastTransition.timestamp < maxTimestamp) {
            val left:  Int = displayModel.timestampToXCoordinate(lastTransition.timestamp)
            val right: Int = displayModel.timestampToXCoordinate(maxTimestamp)
            val z = if (lastTransition.value == 0L) DrawMetrics.WaveformHeight else 0

            // horizontal portion
            g.drawLine(left, top + z, right, top + z)

            // vertical portion
            if (left != 0) {
              g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
            }
          }
        case _ =>
      }
    } catch {
      // If there's only 1 transition in the iterator returned by findTransition,
      // sliding will throw IndexOutOfBoundsException
      case t: IndexOutOfBoundsException =>
        throw t
    }
  }
}
