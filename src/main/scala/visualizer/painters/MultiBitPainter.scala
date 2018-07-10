package visualizer.painters

import java.awt.{Polygon, Rectangle}

import visualizer.DrawMetrics
import visualizer.models.{InspectionDisplayModel, Waveform}

import scala.swing.Graphics2D

class MultiBitPainter(displayModel: InspectionDisplayModel) extends Painter(displayModel) {
  def paintWaveform(g: Graphics2D, waveform: Waveform, y: Int): Unit = {
    waveform.transitions.sliding(2).foreach { transitionPair =>
      val x0: Int = timestampToXCoordinate(transitionPair(0).timestamp)
      val x1: Int = timestampToXCoordinate(transitionPair(1).timestamp)

      g.drawPolygon(new Polygon(Array(x0, x0 + DrawMetrics.Foo, x1 - DrawMetrics.Foo, x1, x1 - DrawMetrics.Foo, x0 + DrawMetrics.Foo),
        Array(y + DrawMetrics.WaveformHeight / 2, y, y, y + DrawMetrics.WaveformHeight / 2, y + DrawMetrics.WaveformHeight, y + DrawMetrics.WaveformHeight),
        6)
      )

      Painter.drawStringCentered(g, transitionPair(0).value.toString,
        new Rectangle(x0, y, x1 - x0, DrawMetrics.WaveformHeight),
        Painter.Arial)
    }
  }
}
