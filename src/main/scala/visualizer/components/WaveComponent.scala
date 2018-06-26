package visualizer.components

import java.awt.{FontMetrics, Polygon}

import visualizer.models._

import scala.swing._

class WaveComponent(dataModel : InspectionDataModel, displayModel : InspectionDisplayModel)
  extends Component with InspectionDisplayModel.Listener {
  val Foo = 5
  val WaveformHeight = 20
  val WaveformVerticalSpacing = 10
  val Arial = new Font("Arial", 0, 12)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    displayModel.inspectedWaves.zipWithIndex.foreach{ case (waveform : Waveform, row : Int) =>
      val y = row * (WaveformHeight + WaveformVerticalSpacing)

      waveform.transitions.sliding(2).foreach { transitionPair =>
        val x1 : Int = ((transitionPair(0).timestamp) * 2).toInt
        val x2 : Int = ((transitionPair(1).timestamp) * 2).toInt

        g.drawPolygon(new Polygon(Array(x1, x1+Foo, x2-Foo, x2, x2-Foo, x1+Foo),
          Array(y + WaveformHeight / 2, y, y, y + WaveformHeight / 2, y + WaveformHeight, y + WaveformHeight),
          6)
        )

        drawStringCentered(g, transitionPair(0).value.toString,
          new Rectangle(x1, y, (x2 - x1), WaveformHeight),
          Arial)
      }
    }
  }


  def drawStringCentered(g: Graphics2D, text: String, rect: Rectangle, font : Font): Unit = {
    val metrics: FontMetrics = g.getFontMetrics(font)
    val x = rect.x + (rect.width - metrics.stringWidth(text)) / 2
    val y = rect.y + ((rect.height - metrics.getHeight()) / 2) + metrics.getAscent
    g.setFont(font)
    g.drawString(text, x, y)
  }

  listenTo(displayModel)
  reactions += {
    case e : WavesAdded => {
      repaint()
    }
  }

  def wavesAdded = {
    repaint()
  }
}
