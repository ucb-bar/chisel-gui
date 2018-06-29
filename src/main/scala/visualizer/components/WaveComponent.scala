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

  //
  // View
  //
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    displayModel.displayTreeModel.depthFirstIterator.zipWithIndex.foreach { case (node, row) =>

      val y = row * (WaveformHeight + WaveformVerticalSpacing)

      if (node.id >= 0) {
        dataModel.waveforms(node.id).transitions.sliding(2).foreach { transitionPair =>
          val x0 : Int = timeToXCoord(transitionPair(0).timestamp)
          val x1 : Int = timeToXCoord(transitionPair(1).timestamp)

          g.drawPolygon(new Polygon(Array(x0, x0+Foo, x1-Foo, x1, x1-Foo, x0+Foo),
            Array(y + WaveformHeight / 2, y, y, y + WaveformHeight / 2, y + WaveformHeight, y + WaveformHeight),
            6)
          )

          drawStringCentered(g, transitionPair(0).value.toString,
            new Rectangle(x0, y, x1 - x0, WaveformHeight),
            Arial)
        }
      } else { // it's a group!
        // do nothing i think?
      }


    }
  }

  def drawStringCentered(g: Graphics2D, text: String, rect: Rectangle, font : Font): Unit = {
    val metrics: FontMetrics = g.getFontMetrics(font)
    val x = rect.x + (rect.width - metrics.stringWidth(text)) / 2
    val y = rect.y + ((rect.height - metrics.getHeight) / 2) + metrics.getAscent
    g.setFont(font)
    g.drawString(text, x, y)
  }

  //
  // Helper functions
  //

  def timeToXCoord(timestamp : Long): Int = {
    (timestamp * displayModel.scale).toInt
  }

  def computeBounds : Unit = {
    preferredSize = new Dimension(timeToXCoord(dataModel.maxTimestamp), displayModel.inspectedWaves.size * (WaveformHeight + WaveformVerticalSpacing))
    revalidate()
    println(s"(${preferredSize.width}, ${preferredSize.height})")
  }

  //
  // Controller
  //

  listenTo(displayModel)
  reactions += {
    case e : SignalsAdded => wavesAdded
    case e : ScaleChanged => {
      computeBounds
      repaint
    }
  }

  def wavesAdded : Unit = {
    computeBounds
    repaint
  }
}
