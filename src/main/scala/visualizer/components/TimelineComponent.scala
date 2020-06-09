package visualizer.components

import java.awt.{BasicStroke, Color}

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

        var lastLabelLocation = -10000
        while (sieveIndex < timeSieve.length) {
          val timeSum = timeSieve.start(sieveIndex)
          val x = selectedSignalModel.timestampToXCoordinate(timeSieve.timeSum(sieveIndex))

          if (x > lastLabelLocation) {
            g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
            val timeLabel = timeSum + " " + unit
            val stringWidth = metrics.stringWidth(timeLabel) + 10
            val t = selectedSignalModel.xCoordinateToSievedTimestamp(x)
            g.drawString(t.toString + " " + unit, x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
            lastLabelLocation = x + stringWidth
          } else {
            g.drawLine(x, DrawMetrics.MinorTickTop, x, DrawMetrics.TimescaleHeight)
          }
          sieveIndex += 1
        }
      case _ =>
        selectedSignalModel.clock match {
          case Some(clk) if selectedSignalModel.useClock =>
            val ssm = selectedSignalModel
            val start = ((ssm.xCoordinateToSievedTimestamp(visibleRect.x - 100) - clk.initialOffset).max(0L) /
              clk.period) * clk.period + clk.initialOffset

            val end = ssm.xCoordinateToSievedTimestamp(visibleRect.x + visibleRect.width)
            val periodsPerScreen = (end - start) / clk.period
            val periodsPerMajor = periodsPerScreen / 10
            val minor = clk.period

            var periodNumber = start / clk.period
            for (t <- start to end by minor) {
              periodNumber += 1
              val x = ssm.timestampToXCoordinate(t)
              if (periodsPerMajor == 0 || periodNumber % periodsPerMajor == 0) {
                if (selectedSignalModel.useClock && clk.period > 0) {
                  g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
                  g.drawString(periodNumber.toString + "c", x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
                } else {
                  g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
                  g.drawString(t.toString + " " + unit, x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
                }
              } else {
                g.drawLine(x, DrawMetrics.MinorTickTop, x, DrawMetrics.TimescaleHeight)
              }
            }

          case _ =>
            // The -100 in start time keeps labels that are to the left of the window from not being drawn
            // (which causes artifacts when scrolling).  It needs to be bigger than the largest label.
            val ssm = selectedSignalModel
            val start = ssm.xCoordinateToSievedTimestamp(visibleRect.x - 100).max(0L)
            val end = ssm.xCoordinateToSievedTimestamp(visibleRect.x + visibleRect.width)
            val minor = ((end - start) / 100).max(1)
            var periodNumber = start / 100

            for (t <- start to end by minor) {
              periodNumber += 1
              val x = ssm.timestampToXCoordinate(t)
              if (periodNumber % 10 == 0) {
                g.drawLine(x, 5, x, DrawMetrics.TimescaleHeight)
                g.drawString(t.toString + " " + unit, x + 3, DrawMetrics.MinorTickTop - metrics.getDescent - 2)
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
