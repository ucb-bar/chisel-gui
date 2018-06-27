package visualizer.components

import visualizer.models._

import scala.swing._
import BorderPanel.Position.Center
import scala.collection.mutable.ArrayBuffer

class InspectionContainer(dataModel : InspectionDataModel, displayModel : InspectionDisplayModel) extends BorderPanel {
  val splitPane = new SplitPane(Orientation.Vertical,
    new ScrollPane(new SignalComponent(dataModel, displayModel)),
    new ScrollPane(new WaveComponent(dataModel, displayModel)) {
      horizontalScrollBar.unitIncrement = 16
      verticalScrollBar.unitIncrement = 16
      horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
      verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    }
  )

  add(splitPane, Center)
}
