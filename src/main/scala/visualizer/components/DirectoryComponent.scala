package visualizer.components

import visualizer.models._

import scala.swing._
import scala.swing.event.ButtonClicked

class DirectoryComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends BoxPanel(Orientation.Vertical) {

  //
  // View
  //

  val addSymbolsButton = new Button("Add")

  val listView = new ListView[String]() {
    listData = dataModel.allWaves.keySet.toSeq
  }
  val symbolList = new ScrollPane(listView)

  contents += symbolList
  contents += addSymbolsButton

  //
  // Controller
  //

  def update : Unit = {
    listView.listData = dataModel.allWaves.keySet.toSeq
    repaint()
  }

  listenTo(addSymbolsButton)
  reactions += {
    case ButtonClicked(`addSymbolsButton`) => {
      listView.selection.items.foreach { item =>
        displayModel.addWave(dataModel.allWaves(item), this)
      }
    }
  }
}
