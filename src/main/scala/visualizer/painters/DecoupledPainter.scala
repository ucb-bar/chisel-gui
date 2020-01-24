package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.config.DrawMetrics._
import visualizer.models._

import scala.swing.Graphics2D

class DecoupledPainter(selectedSignalModel: SelectedSignalModel) extends Painter(selectedSignalModel) {
  def paintWaveform(g:            Graphics2D,
                    visibleRect:  Rectangle,
                    top:          Int,
                    node:         GenericTreeNode,
                    maxTimestamp: Long): Unit = {

    def paintSignal(wave: Wave, startTimestamp: Long): Unit = {
      var index = wave.findStartIndex(startTimestamp)

      while (index < wave.length) {
        val left:  Int = selectedSignalModel.timestampToXCoordinate(wave.start(index))
        val right: Int = selectedSignalModel.timestampToXCoordinate(wave.end(index))
        val value = wave.value(index)
        drawSegment(g, left, right, top, value)
        index += 1
      }
    }

    node match {
      case waveFormNode: WaveFormNode =>
        waveFormNode.signal match {
          case decoupledSignalGroup: DecoupledSignalGroup =>
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
            Waves.get(decoupledSignalGroup.name).foreach { wave =>
              paintSignal(wave, startTimestamp)
            }

          case validSignalGroup: ValidSignalGroup =>
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
            Waves.get(validSignalGroup.name).foreach {
              case wave if wave.nonEmpty =>
                paintSignal(wave, startTimestamp)
            }
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
