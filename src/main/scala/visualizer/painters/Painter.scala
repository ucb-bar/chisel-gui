package visualizer.painters

import java.awt.{FontMetrics, Rectangle}

import visualizer.models.{InspectionDisplayModel, Waveform}

import scala.swing.{Font, Graphics2D}

abstract class Painter(displayModel: InspectionDisplayModel) {
  def paintWaveform(g: Graphics2D, visibleRect: Rectangle, waveform: Waveform, y: Int): Unit

  def timestampToXCoordinate(timestamp: Long): Int = { (timestamp * displayModel.scale).toInt }
  def xCoordinateToTimestamp(coordinate: Int): Long = { (coordinate / displayModel.scale).toLong }
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


}
