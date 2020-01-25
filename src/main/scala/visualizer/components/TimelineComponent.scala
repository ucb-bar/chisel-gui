package visualizer.components

import java.awt.Color

import visualizer._
import visualizer.config.DrawMetrics
import visualizer.models._

import scala.swing._

class TimelineComponent(dataModel: DataModel, selectedSignalModel: SelectedSignalModel) extends Component {

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

    background = new Color(230, 230, 230) //Color.lightGray

    val visibleRect = peer.getVisibleRect
    val metrics = g.getFontMetrics

    selectedSignalModel.timeSieveOpt match {
      case Some(timeSieve) =>
        val startTimestamp = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
        var sieveIndex = timeSieve.findTimeSumIndex(startTimestamp)

        while (sieveIndex < timeSieve.length) {
          val timeSum = timeSieve.start(sieveIndex)
          val x = selectedSignalModel.timestampToXCoordinate(timeSieve.timeSum(sieveIndex))
          g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
          g.drawString(timeSum + " " + unit, x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
          sieveIndex += 1
        }
      case _ =>
        selectedSignalModel.clock match {
          case Some(clk) if selectedSignalModel.useClock =>
            val startTime: Long = math.max(
              ((visibleRect.x - 100) / selectedSignalModel.scale).toLong - clk.initialOffset,
              0
            ) / clk.period * clk.period + clk.initialOffset
            val endTime: Long = ((visibleRect.x + visibleRect.width) / selectedSignalModel.scale).toLong

            for (ts: Long <- startTime until endTime by clk.period) {
              val x: Int = (ts * selectedSignalModel.scale).toInt
              if ((((ts - clk.initialOffset) / clk.period) / selectedSignalModel.clkMinorTickInterval) % 5 == 0) {
                g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
                g.drawString(((ts - clk.initialOffset) / clk.period).toString,
                             x + 3,
                             DrawMetrics.MinorTickTop - metrics.getDescent - 2)
              } else {
                g.drawLine(x, DrawMetrics.MinorTickTop, x, DrawMetrics.TimescaleHeight)
              }
            }
          case _ =>
            // The -100 in start time keeps labels that are to the left of the window from not being drawn
            // (which causes artifacts when scrolling).  It needs to be bigger than the largest label.
            val startTime
              : Long = math.max(((visibleRect.x - 100) / selectedSignalModel.scale).toLong, 0) / selectedSignalModel.minorTickInterval * selectedSignalModel.minorTickInterval
            val endTime: Long = ((visibleRect.x + visibleRect.width) / selectedSignalModel.scale).toLong

            for (ts: Long <- startTime until endTime by selectedSignalModel.minorTickInterval) {
              val x: Int = (ts * selectedSignalModel.scale).toInt
              if ((ts / selectedSignalModel.minorTickInterval) % 10 == 0) {
                g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
                g.drawString((ts / unitMagnitude).toString + " " + unit,
                             x + 3,
                             DrawMetrics.MinorTickTop - metrics.getDescent - 2)
              } else {
                g.drawLine(x, DrawMetrics.MinorTickTop, x, DrawMetrics.TimescaleHeight)
              }
            }
        }
    }

    // Underline timeline. Separates timeline from the wave display
    g.drawLine(visibleRect.x,
               DrawMetrics.TimescaleHeight,
               visibleRect.x + visibleRect.width,
               DrawMetrics.TimescaleHeight)

    // TODO: Draw markers and cursor
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(dataModel, selectedSignalModel)
  reactions += {
    case _: SignalsChanged | _: MaxTimestampChanged =>
      computeBounds()
      repaint()
    case _: ScaleChanged => scaleChanged()
    case _: TimeUnitsChanged =>
      repaint()
  }

  def computeBounds(): Unit = {
    preferredSize = new Dimension((dataModel.maxTimestamp * selectedSignalModel.scale).toInt, preferredSize.height)
    revalidate()
  }

  private def scaleChanged(): Unit = {
    val femtoSecondsPerTimeUnit = math.pow(10, dataModel.timescale + 15).toLong
    val minorTickIntervalFs = selectedSignalModel.minorTickInterval * femtoSecondsPerTimeUnit
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
