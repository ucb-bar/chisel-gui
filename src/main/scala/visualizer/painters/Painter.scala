package visualizer.painters

import java.awt.{FontMetrics, Polygon, Rectangle}

import visualizer.DrawMetrics
import visualizer.models.{GenericTreeNode, SelectedSignalModel, WaveFormNode}

import scala.swing.{Font, Graphics2D}

abstract class Painter(selectedSignalModel: SelectedSignalModel) {
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
}
