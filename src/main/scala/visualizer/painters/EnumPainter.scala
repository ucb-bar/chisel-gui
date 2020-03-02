package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.config.DrawMetrics._
import visualizer.config.{ColorTable, DrawMetrics}
import visualizer.models._

import scala.collection.mutable
import scala.swing.Graphics2D

class EnumPainter(selectedSignalModel: SelectedSignalModel, definition: mutable.HashMap[BigInt, String])
    extends Painter(selectedSignalModel) {

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
              val label = definition.get(value) match {
                case Some(enumName) =>
                  s"$enumName(${formatter(value)})"
                case _ =>
                  formatter(value)
              }

              drawLabel(g, labelLeft, labelRight, top, label)
              lastTransitionValue = value
              index += 1
            }
        }

      case _ =>
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

  def drawSegment(g: Graphics2D, left: Int, right: Int, top: Int, state: BigInt): Unit = {
    state match {
      case DecoupledSignalGroup.Fired =>
        g.setColor(FireColor)
        g.fillPolygon(Painter.hexagon(left, right, top))
      case DecoupledSignalGroup.Ready =>
        g.setColor(ReadySetColor)
        g.fillPolygon(Painter.hexagon(left, right, top))
      case DecoupledSignalGroup.Valid =>
        g.setColor(ValidSetColor)
        g.fillPolygon(Painter.hexagon(left, right, top))
      case DecoupledSignalGroup.Busy =>
        g.setColor(Color.gray)
        g.drawLine(left, top + WaveformHeight / 2, right, top + WaveformHeight / 2)
      case _ =>
        g.setColor(Color.gray)
        g.drawLine(left, top + WaveformHeight / 2, right, top + WaveformHeight / 2)
    }
  }
}
