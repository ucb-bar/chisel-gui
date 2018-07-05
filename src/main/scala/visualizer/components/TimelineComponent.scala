package visualizer.components

import visualizer._
import visualizer.models._

import scala.swing._

class TimelineComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends Component {

  // Constants
  val MinorTicksPerMajor = 10
  val TimescaleHeight = 25
  val MinorTickTop = 18

  // Variables
  var unitMagnitude: Long = 1
  var unit = "s"


  // Initializing?
  preferredSize = new Dimension(200, TimescaleHeight)
  scaleChanged


  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect
    val metrics = g.getFontMetrics

    // The -100 in start time keeps labels that are to the left of the window from not being drawn
    // (which causes artifacts when scrolling).  It needs to be bigger than the largest label.
    val startTime: Long = math.max(((visibleRect.x - 100) / displayModel.scale).toLong, 0) / displayModel.minorTickInterval * displayModel.minorTickInterval
    val endTime: Long = ((visibleRect.x + visibleRect.width) / displayModel.scale).toLong

    for (ts: Long <- startTime until endTime by displayModel.minorTickInterval) {
      val x: Int = (ts * displayModel.scale).toInt
      if ((ts / displayModel.minorTickInterval) % 10 == 0) {
        g.drawLine(x, 5, x, TimescaleHeight)
        g.drawString((ts / unitMagnitude).toString + " " + unit, x + 3, MinorTickTop - metrics.getDescent - 2)
      } else {
        g.drawLine(x, MinorTickTop, x, TimescaleHeight)
      }
    }

    // Underline timeline. separates timeline from the wave display
    g.drawLine(visibleRect.x, TimescaleHeight, visibleRect.x + visibleRect.width, TimescaleHeight)

    // TODO: Draw markers and cursor

  }




  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////

  listenTo(displayModel)
  reactions += {
    case e: ScaleChanged => scaleChanged
  }

  private def scaleChanged: Unit = {
    val femtoSecondsPerTimeUnit = math.pow(10, dataModel.timescale + 15).toLong
    val minorTickIntervalFs = displayModel.minorTickInterval * femtoSecondsPerTimeUnit
    val unitMagnitudeFs = if (minorTickIntervalFs < 100L) {
      unit = "fs"
      1L
    } else if (minorTickIntervalFs < 100000L) {
      unit = "ps"
      1000L
    } else if (minorTickIntervalFs < 100000000L) {
      unit = "ns"
      1000000L
    } else if (minorTickIntervalFs < 100000000000L) {
      unit = "us"
      1000000000L
    } else if (minorTickIntervalFs < 100000000000000L) {
      unit = "ms"
      1000000000000L
    } else {
      unit = "s"
      1000000000000000L
    }

    unitMagnitude = unitMagnitudeFs / femtoSecondsPerTimeUnit
    preferredSize = new Dimension((dataModel.maxTimestamp * displayModel.scale).toInt, preferredSize.height)
    revalidate
    repaint
  }
}
