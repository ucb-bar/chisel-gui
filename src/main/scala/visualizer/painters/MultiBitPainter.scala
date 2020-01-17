package visualizer.painters

import java.awt.Rectangle

import visualizer.config.{ColorTable, DrawMetrics}
import visualizer.models._

import scala.swing.Graphics2D

/** This displays signals data that takes on integer values other than one and zero
  *
  * @param selectedSignalModel data to construct this kind of bar
  */
class MultiBitPainter(selectedSignalModel: SelectedSignalModel) extends Painter(selectedSignalModel) {

  def paintWaveform(g:            Graphics2D,
                    visibleRect:  Rectangle,
                    top:          Int,
                    node:         GenericTreeNode,
                    maxTimestamp: Long): Unit = {

    node match {
      case waveFormNode: WaveFormNode =>
        waveFormNode.signal match {
          case pureSignal: PureSignal =>
            val formatter = selectedSignalModel.waveDisplaySettings(pureSignal.name).dataFormat match {
              case Some(PlotFormat) => PlotFormat
              case Some(format)     => format
              case None             => DecFormat
            }
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
            g.setColor(ColorTable(ColorTable.waveSignal))

            var lastTransitionValue = BigInt(0)

            // Only paint from first transition at or before the start timestamp
            // up until the first transition after the end timestamp
            try {
              pureSignal.waveform.get
                .findTransition(startTimestamp)
                .sliding(2)
                .takeWhile { transitionPair =>
                  selectedSignalModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
                }
                .foreach {
                  case transition1 :: transition2 :: Nil =>
                    val left:  Int = selectedSignalModel.timestampToXCoordinate(transition1.timestamp)
                    val right: Int = selectedSignalModel.timestampToXCoordinate(transition2.timestamp)

                    if (formatter == PlotFormat) {
                      Painter.drawPlotLine(g, left, transition1.value, right, transition2.value, pureSignal, top)
                    } else {
                      g.drawPolygon(Painter.hexagon(left, right, top))
                    }

                    val labelLeft = math.max(visibleRect.x, left + DrawMetrics.Foo)
                    val labelRight = math.min(visibleRect.x + visibleRect.width, right - DrawMetrics.Foo)
                    drawLabel(g, labelLeft, labelRight, top, formatter(transition1.value))
                    lastTransitionValue = transition2.value

                  case transition :: Nil =>
                    val left:  Int = selectedSignalModel.timestampToXCoordinate(0L)
                    val right: Int = selectedSignalModel.timestampToXCoordinate(transition.timestamp)

                    if (left < right) {
                      g.drawPolygon(Painter.hexagon(left, right, top))
                    }

                    val labelLeft = math.max(visibleRect.x, left + DrawMetrics.Foo)
                    val labelRight = math.min(visibleRect.x + visibleRect.width, right - DrawMetrics.Foo)
                    drawLabel(g, labelLeft, labelRight, top, formatter(transition.value))
                }

              pureSignal.waveform.get.transitions.lastOption match {
                case Some(lastTransition) =>
                  if (lastTransition.timestamp < maxTimestamp) {
                    val left:  Int = selectedSignalModel.timestampToXCoordinate(lastTransition.timestamp)
                    val right: Int = selectedSignalModel.timestampToXCoordinate(maxTimestamp)
                    val z = if (lastTransition.value == 0L) DrawMetrics.WaveformHeight else 0

                    if (formatter == PlotFormat) {
                      Painter.drawPlotLine(g, left, lastTransitionValue, right, lastTransition.value, pureSignal, top)
                    } else {
                      if (left < right) {
                        Painter.drawHalfHexagon(g, left, right, top)
                      }
                    }

                    val labelLeft = math.max(visibleRect.x, left + DrawMetrics.Foo)
                    val labelRight = math.min(visibleRect.x + visibleRect.width, right - DrawMetrics.Foo)
                    drawLabel(g, labelLeft, labelRight, top, formatter(lastTransition.value))
                  }
                case _ =>
              }

            } catch {
              // If there's only 1 transition in the iterator returned by findTransition,
              // sliding will throw IndexOutOfBoundsException
              case _: IndexOutOfBoundsException =>
            }
        }

      case _ =>
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
