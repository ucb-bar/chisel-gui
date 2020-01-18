package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.config.DrawMetrics._
import visualizer.models._

import scala.swing.Graphics2D

class ReadyValidPainter(selectedSignalModel: SelectedSignalModel) extends Painter(selectedSignalModel) {
  def paintWaveform(g:            Graphics2D,
                    visibleRect:  Rectangle,
                    top:          Int,
                    node:         GenericTreeNode,
                    maxTimestamp: Long): Unit = {
    node match {
      case waveFormNode: WaveFormNode =>
        waveFormNode.signal match {
          case combinedSignal: CombinedSignal =>
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)

            try {
              combinedSignal.waveform.get
                .findTransition(startTimestamp)
                .sliding(2)
                .takeWhile { transitionPair =>
                  selectedSignalModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
                }
                .foreach { transitionPair =>
                  // length could be 1 if findTransition(startTimestamp) has length 1
                  if (transitionPair.length == 2) {
                    val left:  Int = selectedSignalModel.timestampToXCoordinate(transitionPair.head.timestamp)
                    val right: Int = selectedSignalModel.timestampToXCoordinate(transitionPair.last.timestamp)

                    assert(transitionPair.head.value.length == 2)
                    drawSegment(g,
                                left,
                                right,
                                top,
                                transitionPair.head.value(0) == 1,
                                transitionPair.head.value(1) == 1)
                  }
                }
            } catch {
              // If there's only 1 transition in the iterator returned by findTransition,
              // sliding will throw IndexOutOfBoundsException
              case _: IndexOutOfBoundsException =>
            }
        }
    }
  }

  def drawSegment(g: Graphics2D, left: Int, right: Int, top: Int, ready: Boolean, valid: Boolean): Unit = {
    (ready, valid) match {
      case (true, true) =>
        g.setColor(FireColor)
        g.fillPolygon(Painter.hexagon(left, right, top))
      case (true, false) =>
        g.setColor(ReadySetColor)
        g.fillPolygon(Painter.hexagon(left, right, top))
      case (false, true) =>
        g.setColor(ValidSetColor)
        g.fillPolygon(Painter.hexagon(left, right, top))
      case (false, false) =>
        g.setColor(Color.gray)
        g.drawLine(left, top + WaveformHeight / 2, right, top + WaveformHeight / 2)
    }
  }
}
