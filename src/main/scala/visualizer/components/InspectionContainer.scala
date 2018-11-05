package visualizer.components

import javax.swing.{BorderFactory, DropMode, SwingUtilities}
import javax.swing.event.{TreeExpansionEvent, TreeExpansionListener}
import javax.swing.tree.{DefaultMutableTreeNode, TreePath}
import scalaswingcontrib.tree.Tree
import visualizer.controllers._
import visualizer.{DrawMetrics, SignalsChanged}
import visualizer.models._

import scala.swing._
import BorderPanel.Position._
import scala.swing.event.MouseClicked

class InspectionContainer(waveFormController: WaveFormController) extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  preferredSize = new Dimension(500, 700)

  val timelineComponent = new TimelineComponent(waveFormController)
  val waveComponent     = new WaveComponent(waveFormController)
  val selectionComponent = new SignalComponent(waveFormController)

  val signalScrollPane: ScrollPane = new ScrollPane(selectionComponent) {
    minimumSize = new Dimension(150, 300)
    border = BorderFactory.createEmptyBorder()

    verticalScrollBar.unitIncrement = 16

    // prevents apple trackpad jittering
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
  }
  val waveScrollPane: ScrollPane = new ScrollPane(waveComponent) {
    preferredSize = new Dimension (550, 700)
    horizontalScrollBar.unitIncrement = 16
    verticalScrollBar.unitIncrement = 16
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    columnHeaderView = timelineComponent
  }
  signalScrollPane.verticalScrollBar.peer.setModel(
    waveScrollPane.verticalScrollBar.peer.getModel
  )

  val splitPane: SplitPane = new SplitPane(Orientation.Vertical,
    new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(timelineComponent.preferredSize.height)
      contents += signalScrollPane
    },
    waveScrollPane
  ) {
    border = BorderFactory.createEmptyBorder()
  }
  add(splitPane, Center)


  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  def setScaleKeepCentered(newScale: Double, source: Component): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect
    val centerTimestamp = ((oldVisibleRect.x + oldVisibleRect.width / 2) / waveFormController.scale).toLong

    waveFormController.setScale(newScale, source)

    val centerX = (centerTimestamp * newScale).toInt
    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = centerX - newVisibleRect.width / 2
    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }

  def zoomIn(source: Component): Unit = {
    setScaleKeepCentered(waveFormController.scale * 1.25, source)
  }

  def zoomOut(source: Component): Unit = {
    setScaleKeepCentered(waveFormController.scale * 0.8, source)
  }

  /**
    * move wave view to end, keeping the current scale
    * @param source component to scroll
    */
  def zoomToEnd(source: Component): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect
    val maxTimestamp = waveFormController.maxTimestamp

    val clockTickWidth = oldVisibleRect.width / waveFormController.scale

    val minTimestamp = (maxTimestamp - clockTickWidth).max(0)

    val centerTimestamp = (maxTimestamp - minTimestamp) / 2 + minTimestamp

    val centerX = (centerTimestamp * waveFormController.scale).toInt

    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = centerX - newVisibleRect.width / 2
    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }

  def removeSignals(source: Component): Unit = {
    waveFormController.removeSelectedSignals(source, waveFormController.tree.selection.paths.iterator)
  }

  def goToEnd(source: Component, steps: Int): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect

    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = (oldVisibleRect.x + steps / waveFormController.scale).toInt

    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }
}
