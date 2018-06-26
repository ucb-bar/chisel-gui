package visualizer.components

import visualizer.models.{InspectionDataModel, InspectionDisplayModel, WavesAdded}

import scala.swing._
import BorderPanel.Position.Center

class SignalComponent(dataModel : InspectionDataModel, displayModel : InspectionDisplayModel)
  extends BorderPanel {

  val signalView = new ListView[String]() {
    listData = displayModel.inspectedWaves.map(_.name)
  }

  add(signalView, Center)




  listenTo(displayModel)
  reactions += {
    case e : WavesAdded => wavesAdded
  }
  def wavesAdded = {
    signalView.listData = displayModel.inspectedWaves.map(_.name)
    repaint()
  }
}
