package visualizer.components

import scalaswingcontrib.tree._
import visualizer.models._

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.ButtonClicked

class DirectoryComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends BoxPanel(Orientation.Vertical) {
  //
  // View
  //

  val addSymbolsButton = new Button("Add")

  val symbolList = new ScrollPane(dataModel.tree)

  contents += symbolList
  contents += addSymbolsButton

  //
  // Controller
  //

  def update: Unit = {
    repaint()
  }

  listenTo(addSymbolsButton)
  reactions += {
    case ButtonClicked(`addSymbolsButton`) => {
      dataModel.tree.selection.cellValues.foreach{v =>
        if (v.waveId >= 0) {
//          displayModel.displayTreeModel.insertUnder(displayModel.RootPath, v, 0)
          displayModel.addSignal(v, this)
        } else {
          displayModel.addModule(v, this)
        }
      }
    }
  }


}
