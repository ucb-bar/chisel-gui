package visualizer.painters

import java.awt.{FontMetrics, Polygon, Rectangle}

import visualizer.DrawMetrics
import visualizer.controllers.WaveFormController
import visualizer.models.{InspectedNode, PureSignal, Signal}

import scala.swing.{Font, Graphics2D}

abstract class Painter(displayModel: WaveFormController) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, top: Int, signal: Signal[_]): Unit
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

  def hexagon(left: Int, right: Int, top: Int): Polygon = {
    val xs = new Array[Int](6)
    xs(0) = left
    xs(1) = left + DrawMetrics.Foo
    xs(2) = right - DrawMetrics.Foo
    xs(3) = right
    xs(4) = xs(2)
    xs(5) = xs(1)

    val ys = new Array[Int](6)
    ys(0) = top + DrawMetrics.WaveformHeight / 2
    ys(1) = top
    ys(2) = top
    ys(3) = ys(0)
    ys(4) = top + DrawMetrics.WaveformHeight
    ys(5) = ys(4)

    new Polygon(xs, ys, 6)
  }
}
