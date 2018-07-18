package visualizer.components

import visualizer.models._

import scala.swing._
import scala.swing.event.ButtonClicked

class DirectoryComponent(
  dataModel: DataModel,
  displayModel: DisplayModel
) extends BoxPanel(Orientation.Vertical) {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val addSymbolsButton = new Button("Add")
  val symbolList = new ScrollPane(dataModel.tree)
  contents += symbolList
  contents += addSymbolsButton

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(addSymbolsButton)
  reactions += {
    case ButtonClicked(`addSymbolsButton`) =>
      dataModel.tree.selection.cellValues.foreach{node =>
        displayModel.addToInspected(node, this)
      }
  }
}
