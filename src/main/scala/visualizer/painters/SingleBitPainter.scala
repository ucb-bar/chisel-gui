package visualizer.painters

import java.awt.{Color, Rectangle}

import visualizer.config.{ColorTable, DrawMetrics}
import visualizer.models._

import scala.swing.Graphics2D

class SingleBitPainter(selectedSignalModel: SelectedSignalModel) extends Painter(selectedSignalModel) {
  def paintWaveform(g:            Graphics2D,
                    visibleRect:  Rectangle,
                    top:          Int,
                    node:         GenericTreeNode,
                    maxTimestamp: Long): Unit = {
    node match {
      case waveFormNode: WaveFormNode =>
        waveFormNode.signal match {
          case pureSignal: PureSignal =>
            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
            g.setColor(ColorTable(ColorTable.waveSignal))

            // Only paint from first transition at or before the start timestamp
            // up until the first transition after the end timestamp
            val wave = Waves(pureSignal.name)
            if (wave.length > 0) {
              var index = wave.findStartIndex(startTimestamp)

              while (index < wave.length) {
                val left: Int = selectedSignalModel.timestampToXCoordinate(wave.start(index))
                val right: Int = selectedSignalModel.timestampToXCoordinate(wave.end(index))
                val value = wave.value(index)
                val z = if (value == 0) DrawMetrics.WaveformHeight else 0

                // horizontal portion
                g.drawLine(left, top + z, right, top + z)

                // vertical portion
                if (left != 0) {
                  g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
                }
                index += 1
              }
            }
        }
    }
  }
}

//class OldSingleBitPainter(selectedSignalModel: SelectedSignalModel) extends Painter(selectedSignalModel) {
//  def paintWaveform(g:            Graphics2D,
//                    visibleRect:  Rectangle,
//                    top:          Int,
//                    node:         GenericTreeNode,
//                    maxTimestamp: Long): Unit = {
//    node match {
//      case waveFormNode: WaveFormNode =>
//        waveFormNode.signal match {
//          case pureSignal: PureSignal =>
//            val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
//            g.setColor(ColorTable(ColorTable.waveSignal))
//
//            // Only paint from first transition at or before the start timestamp
//            // up until the first transition after the end timestamp
//            try {
//              pureSignal.waveform.get
//                .findTransition(startTimestamp)
//                .sliding(2)
//                .takeWhile { transitionPair =>
//                  selectedSignalModel.timestampToXCoordinate(transitionPair.head.timestamp) < visibleRect.x + visibleRect.width
//                }
//                .foreach {
//                  case transition1 :: transition2 :: Nil =>
//                    val left:  Int = selectedSignalModel.timestampToXCoordinate(transition1.timestamp)
//                    val right: Int = selectedSignalModel.timestampToXCoordinate(transition2.timestamp)
//                    val z = if (transition1.value == 0) DrawMetrics.WaveformHeight else 0
//
//                    // horizontal portion
//                    g.drawLine(left, top + z, right, top + z)
//
//                    // vertical portion
//                    if (left != 0) {
//                      g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
//                    }
//                  case transition :: Nil =>
//                    val left:  Int = selectedSignalModel.timestampToXCoordinate(0L)
//                    val right: Int = selectedSignalModel.timestampToXCoordinate(transition.timestamp)
//                    val z = if (transition.value == 0) DrawMetrics.WaveformHeight else 0
//
//                    // horizontal portion
//                    g.drawLine(left, top + z, right, top + z)
//
//                    // vertical portion
//                    if (left != 0) {
//                      g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
//                    }
//                }
//
//              pureSignal.waveform.get.transitions.lastOption match {
//                case Some(lastTransition) =>
//                  if (lastTransition.timestamp < maxTimestamp) {
//                    val left:  Int = selectedSignalModel.timestampToXCoordinate(lastTransition.timestamp)
//                    val right: Int = selectedSignalModel.timestampToXCoordinate(maxTimestamp)
//                    val z = if (lastTransition.value == 0L) DrawMetrics.WaveformHeight else 0
//
//                    // horizontal portion
//                    g.drawLine(left, top + z, right, top + z)
//
//                    // vertical portion
//                    if (left != 0) {
//                      g.drawLine(left, top, left, top + DrawMetrics.WaveformHeight)
//                    }
//                  }
//                case _ =>
//              }
//            } catch {
//              // If there's only 1 transition in the iterator returned by findTransition,
//              // sliding will throw IndexOutOfBoundsException
//              case t: IndexOutOfBoundsException =>
//                throw t
//            }
//        }
//    }
//  }
//}
