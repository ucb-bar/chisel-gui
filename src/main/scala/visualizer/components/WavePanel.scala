package visualizer.components

import java.awt.Rectangle

import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.config.{ColorTable, DrawMetrics}
import visualizer.models._
import visualizer.painters.{DecoupledPainter, MultiBitPainter, SingleBitPainter}

import scala.swing._
import scala.swing.event._

class WavePanel(dataModel: DataModel, selectedSignalModel: SelectedSignalModel, tree: Tree[GenericTreeNode])
    extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  private val multiBitPainter = new MultiBitPainter(selectedSignalModel)
  private val singleBitPainter = new SingleBitPainter(selectedSignalModel)
  private val decoupledPainter = new DecoupledPainter(selectedSignalModel)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    val visibleRect = peer.getVisibleRect

    // Set background color
    background = ColorTable(ColorTable.waveBackground)

    // Draw waveforms
    TreeHelper.viewableDepthFirstIterator(tree).zipWithIndex.foreach {
      case (_:    DirectoryNode, _) =>
      case (node: SignalTreeNode, row) =>
        val y = row * DrawMetrics.WaveformVerticalSpacing + DrawMetrics.WaveformVerticalGap
        node.signal match {
          case signal: PureSignal =>
            if (signal.isBinary) {
              singleBitPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
            } else {
              multiBitPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
            }
          case _: DecoupledSignalGroup =>
            decoupledPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
          case _: ValidSignalGroup =>
            decoupledPainter.paintWaveform(g, visibleRect, y, node, dataModel.maxTimestamp)
        }
      case _ =>
    }

    // Draw grid lines
    drawGridLines(g, visibleRect)

    // Draw markers
    drawMarkers(g, visibleRect)

    // Draw cursor
    g.setColor(ColorTable(ColorTable.waveCursor))
    val cursorX = selectedSignalModel.sievedTimestampToXCoordinate(selectedSignalModel.cursorPosition)
    g.drawLine(cursorX, visibleRect.y, cursorX, visibleRect.y + visibleRect.height)
  }

  def drawMarkers(g: Graphics2D, visibleRect: Rectangle): Unit = {
    val startTime = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x)
    val endTime = selectedSignalModel.xCoordinateToTimestamp(visibleRect.x + visibleRect.width)
    val startIndex = selectedSignalModel.getMarkerAtTime(startTime)
    val endIndex = selectedSignalModel.getMarkerAtTime(endTime)

    g.setColor(ColorTable(ColorTable.waveMarker))
    selectedSignalModel.markers.slice(startIndex, endIndex + 1).foreach { marker =>
      val x = selectedSignalModel.sievedTimestampToXCoordinate(marker.timestamp)
      g.drawLine(x, 0, x, visibleRect.y + visibleRect.height)
      g.drawString(marker.description,
                   x + DrawMetrics.MarkerNameXOffset,
                   visibleRect.y + visibleRect.height + DrawMetrics.MarkerNameYOffset)

    }
  }

  def drawGridLines(g: Graphics2D, visibleRect: Rectangle): Unit = {
    val startTime = selectedSignalModel.xCoordinateToSievedTimestamp(visibleRect.x)
    val endTime = selectedSignalModel.xCoordinateToSievedTimestamp(visibleRect.x + visibleRect.width)
    val clockPeriod: Long = ChiselGUI.testerOpt match {
      case Some(tester) => tester.clockInfoList.head.period * 10
      case _            => 10L
    }

    val startGridLineX = (startTime / clockPeriod) * clockPeriod
    val endGridLineX = (endTime / clockPeriod) * clockPeriod

    g.setColor(ColorTable(ColorTable.waveGrid))
    for (time <- startGridLineX to endGridLineX by clockPeriod) {
      val x = selectedSignalModel.timestampToXCoordinate(time)
      g.drawLine(x, 0, x, visibleRect.y + visibleRect.height)
    }
  }

  //
  // Helper functions
  //
  def computeBounds(): Unit = {
    preferredSize = new Dimension(
      selectedSignalModel.timestampToXCoordinate(dataModel.maxTimestamp),
      TreeHelper.viewableDepthFirstIterator(tree).size
        * DrawMetrics.WaveformVerticalSpacing
    )
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(dataModel, selectedSignalModel, tree)
  listenTo(mouse.clicks, mouse.moves)
  reactions += {
    case _: SignalsChanged | _: ScaleChanged | _: CursorSet | _: MaxTimestampChanged =>
      computeBounds()
      repaint()
    case _: WaveFormatChanged =>
      repaint()
    case e: MarkerChanged =>
      if (e.timestamp < 0)
        repaint()
      else
        repaint(
          new Rectangle(selectedSignalModel.timestampToXCoordinate(e.timestamp) - 1, 0, 2, peer.getVisibleRect.height)
        )
    case e: MousePressed =>
      val timestamp = selectedSignalModel.xCoordinateToTimestamp(e.peer.getX)
      if (selectedSignalModel.cursorPosition != selectedSignalModel.selectionStart)
        repaint()
      if (!e.peer.isShiftDown) {
        selectedSignalModel.selectionStart = timestamp
      }
      selectedSignalModel.setCursorPosition(timestamp)
    // selectedSignalModel.adjustingCursor = true
    case _: MouseReleased => // selectedSignalModel.adjustingCursor = false
    case e: MouseDragged =>
      val timestamp = selectedSignalModel.xCoordinateToTimestamp(e.peer.getX)
      selectedSignalModel.setCursorPosition(timestamp)
      peer.scrollRectToVisible(new Rectangle(e.peer.getX, e.peer.getY, 1, 1))
  }
}
