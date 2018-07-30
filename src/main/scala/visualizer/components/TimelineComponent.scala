package visualizer.components

import visualizer._
import visualizer.models._

import scala.swing._

class TimelineComponent(dataModel: DataModel, displayModel: DisplayModel) extends Component {

  var unitMagnitude: Long = 1
  var unit = "s"

  // Initializing?
  preferredSize = new Dimension(200, DrawMetrics.TimescaleHeight)
  scaleChanged()

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect
    val metrics = g.getFontMetrics

    displayModel.clock match {
      case Some(clk) if displayModel.useClock =>
        val startTime: Long = math.max(((visibleRect.x - 100) / displayModel.scale).toLong - clk.initialOffset, 0) / clk.period * clk.period + clk.initialOffset
        val endTime: Long = ((visibleRect.x + visibleRect.width) / displayModel.scale).toLong

        for (ts: Long <- startTime until endTime by clk.period) {
          val x: Int = (ts * displayModel.scale).toInt
          if ((((ts -  clk.initialOffset) / clk.period) / displayModel.clkMinorTickInterval) % 5 == 0) {
            g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
            g.drawString(((ts - clk.initialOffset) / clk.period).toString, x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
          } else {
            g.drawLine(x, DrawMetrics.MinorTickTop, x, DrawMetrics.TimescaleHeight)
          }
        }
      case _ =>
        // The -100 in start time keeps labels that are to the left of the window from not being drawn
        // (which causes artifacts when scrolling).  It needs to be bigger than the largest label.
        val startTime: Long = math.max(((visibleRect.x - 100) / displayModel.scale).toLong, 0) / displayModel.minorTickInterval * displayModel.minorTickInterval
        val endTime: Long = ((visibleRect.x + visibleRect.width) / displayModel.scale).toLong

        for (ts: Long <- startTime until endTime by displayModel.minorTickInterval) {
          val x: Int = (ts * displayModel.scale).toInt
          if ((ts / displayModel.minorTickInterval) % 10 == 0) {
            g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
            g.drawString((ts / unitMagnitude).toString + " " + unit, x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
          } else {
            g.drawLine(x, DrawMetrics.MinorTickTop, x, DrawMetrics.TimescaleHeight)
          }
        }
    }

    // Underline timeline. Separates timeline from the wave display
    g.drawLine(visibleRect.x, DrawMetrics.TimescaleHeight, visibleRect.x + visibleRect.width, DrawMetrics.TimescaleHeight)

    // TODO: Draw markers and cursor
  }


  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(dataModel, displayModel)
  reactions += {
    case _: SignalsChanged | _: MaxTimestampChanged =>
      computeBounds()
      repaint()
    case _: ScaleChanged => scaleChanged()
    case _: TimeUnitsChanged =>
      repaint()
  }

  def computeBounds(): Unit = {
    preferredSize = new Dimension((dataModel.maxTimestamp * displayModel.scale).toInt, preferredSize.height)
    revalidate()
  }

  private def scaleChanged(): Unit = {
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
    computeBounds()
    repaint()
  }
}
