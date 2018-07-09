package visualizer.components

import java.awt.Rectangle

import visualizer.{DrawMetrics, ScaleChanged}
import visualizer.models._

import scala.swing._
import BorderPanel.Position._

class InspectionContainer(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val timelineComponent = new TimelineComponent(dataModel, displayModel)
  val waveComponent = new WaveComponent(dataModel, displayModel)
  val signalComponent = new SignalComponent(dataModel, displayModel)

  val scrollPane: ScrollPane = new ScrollPane(waveComponent) {
    horizontalScrollBar.unitIncrement = 16
    verticalScrollBar.unitIncrement = 16
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    columnHeaderView = timelineComponent
  }
  val splitPane = new SplitPane(Orientation.Vertical,
    new BorderPanel {
      add(Swing.VStrut(timelineComponent.preferredSize.height), North)
      add(new ScrollPane(signalComponent), Center)
      add(Swing.VStrut(scrollPane.horizontalScrollBar.preferredSize.height), South)
    },
    scrollPane
  )
  add(splitPane, Center)


  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////

  def setScaleKeepCentered(newScale: Double, source: Component): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect
    val centerTimestamp = ((oldVisibleRect.x + oldVisibleRect.width / 2) / displayModel.scale).toLong

    displayModel.setScale(newScale, source)

    val centerX = (centerTimestamp * newScale).toInt
    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = centerX - newVisibleRect.width / 2
    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }

  def zoomIn(source: Component): Unit = {
    setScaleKeepCentered(displayModel.scale * 1.25, source)
  }

  def zoomOut(source: Component): Unit = {
    setScaleKeepCentered(displayModel.scale * 0.8, source)
  }
}
