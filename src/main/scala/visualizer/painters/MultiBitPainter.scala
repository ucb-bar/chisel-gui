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
            val minLastEndTimestamp =
              maxTimestamp.min(selectedSignalModel.xCoordinateToTimestamp(visibleRect.x + visibleRect.width))

            g.setColor(ColorTable(ColorTable.waveSignal))

            var lastTransitionValue = BigInt(0)

            // Only paint from first transition at or before the start timestamp
            // up until the first transition after the end timestamp
//            val wave = Waves(pureSignal.name)
            val wave = getWave(pureSignal.name, startTimestamp).get
            var index = wave.findStartIndex(startTimestamp)
            var useHalfHexagon = false
            while (index < wave.length) {
              val left: Int = selectedSignalModel.timestampToXCoordinate(wave.start(index))
              val right: Int = if (index < wave.length - 1 || selectedSignalModel.timeSieveOpt.isDefined) {
                selectedSignalModel.timestampToXCoordinate(wave.end(index))
              } else {
                val lastTime = minLastEndTimestamp.max(wave.end(index))
                if (lastTime > wave.end(index)) useHalfHexagon = true
                selectedSignalModel.timestampToXCoordinate(lastTime)
              }
              val value = wave.value(index)

              if (formatter == PlotFormat) {
                val nextValue = if (index < wave.length - 1) wave.value(index + 1) else value
                Painter.drawPlotLine(g, left, value, right, nextValue, pureSignal, top)
              } else {
                if (useHalfHexagon) {
                  Painter.drawHalfHexagon(g, left, right, top)
                } else {
                  g.drawPolygon(Painter.hexagon(left, right, top))
                }
              }

              val labelLeft = math.max(visibleRect.x, left + DrawMetrics.Foo)
              val labelRight = math.min(visibleRect.x + visibleRect.width, right - DrawMetrics.Foo)
              drawLabel(g, labelLeft, labelRight, top, formatter(value))
              lastTransitionValue = value
              index += 1
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
