package visualizer.painters

import java.awt.{FontMetrics, Polygon, Rectangle}

import visualizer.config.DrawMetrics
import visualizer.models.{GenericTreeNode, PureSignal, SelectedSignalModel, Wave, Waves}

import scala.swing.{Font, Graphics2D}

abstract class Painter(selectedSignalModel: SelectedSignalModel) {
  def getWave(name: String, startTime: Long): Option[Wave] = {
    Waves.get(name) match {
      case Some(wave) =>
        selectedSignalModel.timeSieveOpt match {
          case Some(timeSieve) =>
            val strainedWave = timeSieve.strain(wave, startTime)
            Some(strainedWave)
          case _ =>
            Some(wave)
        }
      case _ => None
    }
  }

  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, top: Int, node: GenericTreeNode, maxTimestamp: Long): Unit
}

object Painter {
  val Arial = new Font("Arial", 0, 12)

  def drawStringCentered(g: Graphics2D, text: String, rect: Rectangle, font: Font): Unit = {
    val metrics: FontMetrics = g.getFontMetrics(font)
    val x = rect.x + (rect.width - metrics.stringWidth(text)) / 2
    val y = rect.y + ((rect.height - metrics.getHeight) / 2) + metrics.getAscent
    g.setFont(font)
    g.drawString(text, x, y)
  }

  /** Creates a wide hexagon that spans left to right
    *
    * @param left    left most point
    * @param right   right most point
    * @param bottom  bottom y value of hexagon
    * @return
    */
  def hexagon(left: Int, right: Int, bottom: Int): Polygon = {
    if(left + DrawMetrics.Foo * 8 < right ) {
      val xs = new Array[Int](6)
      xs(0) = left
      xs(1) = left + DrawMetrics.Foo
      xs(2) = right - DrawMetrics.Foo
      xs(3) = right
      xs(4) = xs(2)
      xs(5) = xs(1)

      val ys = new Array[Int](6)
      ys(0) = bottom + DrawMetrics.WaveformHeight / 2
      ys(1) = bottom
      ys(2) = bottom
      ys(3) = ys(0)
      ys(4) = bottom + DrawMetrics.WaveformHeight
      ys(5) = ys(4)

      new Polygon(xs, ys, 6)
    } else {
      val xs = new Array[Int](4)
      xs(0) = left
      xs(1) = left
      xs(2) = right
      xs(3) = right

      val ys = new Array[Int](4)
      ys(0) = bottom + DrawMetrics.WaveformHeight
      ys(1) = bottom
      ys(2) = bottom
      ys(3) = bottom + DrawMetrics.WaveformHeight

      new Polygon(xs, ys, 4)
    }
  }

  /** Draws a half hexagon, with the right side open
    * Used to
    *
    * @param g         graphics context
    * @param left      left-most point to draw (left point of hexagon
    * @param right     right most point of open hexagon
    * @param bottom    bottom point of hexagon
    */
  def drawHalfHexagon(g: Graphics2D, left: Int, right: Int, bottom: Int): Unit = {
    val nextX = left + DrawMetrics.Foo
    val topY = bottom + DrawMetrics.WaveformHeight
    val midY = bottom + DrawMetrics.WaveformHeight / 2

    g.drawLine(left, midY, nextX, topY)
    g.drawLine(left, midY, nextX, bottom)
    g.drawLine(nextX, topY, right, topY)
    g.drawLine(nextX, bottom, right, bottom)
  }

  def drawPlotLine(g:      Graphics2D,
                   x1:     Int,
                   value1: BigInt,
                   x2:     Int,
                   value2: BigInt,
                   signal: PureSignal,
                   top:    Int): Unit = {
    val y1 = DrawMetrics.WaveformHeight - (signal.scaledValue(value1) * DrawMetrics.WaveformHeight).toInt + top
    val y2 = DrawMetrics.WaveformHeight - (signal.scaledValue(value2) * DrawMetrics.WaveformHeight).toInt + top
    g.drawLine(x1, y1, x2, y2)
  }
}
