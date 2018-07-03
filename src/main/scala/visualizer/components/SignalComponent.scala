package visualizer.components

import visualizer.models._

import scala.swing._
import BorderPanel.Position.Center

class SignalComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel)
  extends BorderPanel {

  val signalView = displayModel.tree

  add(signalView, Center)


  listenTo(displayModel)
  reactions += {
    case e: SignalsAdded => signalsAdded
  }
  def signalsAdded: Unit = { }

}
