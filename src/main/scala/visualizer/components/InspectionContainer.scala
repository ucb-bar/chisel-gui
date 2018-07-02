package visualizer.components

import visualizer.models._

import scala.swing._
import BorderPanel.Position._
import scala.collection.mutable.ArrayBuffer

class InspectionContainer(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends BorderPanel {
  val timelineComponent = new TimelineComponent(dataModel, displayModel)
  val scrollPane = new ScrollPane(new WaveComponent(dataModel, displayModel)) {
    horizontalScrollBar.unitIncrement = 16
    verticalScrollBar.unitIncrement = 16
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    columnHeaderView = timelineComponent
  }

  val splitPane = new SplitPane(Orientation.Vertical,
    new BorderPanel {
      add(Swing.VStrut(timelineComponent.preferredSize.height), North)
      add(new ScrollPane(new SignalComponent(dataModel, displayModel)), Center)
      add(Swing.VStrut(scrollPane.horizontalScrollBar.preferredSize.height), South)
    },
    scrollPane
  )

  add(splitPane, Center)
}
