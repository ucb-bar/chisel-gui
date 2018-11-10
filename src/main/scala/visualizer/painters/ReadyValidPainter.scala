package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.controllers.WaveFormController
import visualizer.models._

import scala.swing.Graphics2D

class ReadyValidPainter(displayModel: WaveFormController) extends Painter(displayModel) {
  private val FireColor     = new Color(152, 251, 152)
  private val ReadySetColor = new Color(255, 240, 106)
  private val ValidSetColor = new Color(255, 208, 98)
  private val OpenColor     = Color.gray

  def stateToColor(readyValidState: ReadyValidState): Color = {
    readyValidState match {
      case ReadValidStates.Fired    => FireColor
      case ReadValidStates.ReadySet => ReadySetColor
      case ReadValidStates.ValidSet => ValidSetColor
      case ReadValidStates.Open     => OpenColor
      case _ => OpenColor
    }
  }

  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, top: Int, untypedWaveform: Waveform[_]): Unit = {
    //TODO: get the following code working again

    val combinedSignal = untypedWaveform.asInstanceOf[CombinedSignal]
    val startTimestamp = displayModel.xCoordinateToTimestamp(visibleRect.x)
    try {
      combinedSignal.waveform.findTransition(startTimestamp).sliding(2).takeWhile { transitionPair =>
        displayModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
      }.foreach { transitionPair =>
        // length could be 1 if findTransition(startTimestamp) has length 1
        if (transitionPair.length == 2) {
          val left: Int = displayModel.timestampToXCoordinate(transitionPair.head.timestamp)
          val right: Int = displayModel.timestampToXCoordinate(transitionPair.last.timestamp)

          g.setColor(stateToColor(transitionPair.last.value))
          g.fillPolygon(Painter.hexagon(left, right, top))
        }
      }
    } catch {
      // If there's only 1 transition in the iterator returned by findTransition,
      // sliding will throw IndexOutOfBoundsException
      case _: IndexOutOfBoundsException =>
    }
  }
}