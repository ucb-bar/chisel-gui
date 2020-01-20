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

    def paintSignal(waveform: Waveform[BigInt], startTimestamp: Long): Unit = {
      try {
        waveform
          .findTransition(startTimestamp)
          .sliding(2)
          .takeWhile { transitionPair =>
            selectedSignalModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
          }
          .foreach {
            case t1 :: t2 :: Nil =>
              val left: Int = selectedSignalModel.timestampToXCoordinate(t1.timestamp)
              val right: Int = selectedSignalModel.timestampToXCoordinate(t2.timestamp)

              drawSegment(g, left, right, top, t1.value)
            case t1 :: Nil =>
            case _ =>
          }
      } catch {
        // If there's only 1 transition in the iterator returned by findTransition,
        // sliding will throw IndexOutOfBoundsException
        case _: IndexOutOfBoundsException =>
      }
    }

    node match {
      case waveFormNode: WaveFormNode =>
        waveFormNode.signal match {
          case decoupledSignalGroup: DecoupledSignalGroup =>
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
            decoupledSignalGroup.waveform.foreach { waveForm =>
              paintSignal(waveForm, startTimestamp)
            }

          case validSignalGroup: ValidSignalGroup =>
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
            validSignalGroup.waveform.foreach { waveForm =>
              paintSignal(waveForm, startTimestamp)
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
