package visualizer.components

import javax.swing.tree.TreePath
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.Tree
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
  val tree: Tree[DirectoryNode] = new Tree[DirectoryNode] {
    model = dataModel.directoryTreeModel
    renderer = Tree.Renderer(_.name)
    showsRootHandles = true
  }

  val addSymbolsButton = new Button("Add")
  val symbolList = new ScrollPane(tree)
  contents += symbolList
  contents += addSymbolsButton

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(addSymbolsButton)
  listenTo(tree)
  reactions += {
    case ButtonClicked(`addSymbolsButton`) =>
      tree.selection.cellValues.foreach{node =>
        displayModel.addToInspected(node, this)
      }
    case e: TreeNodesInserted[_] =>
      if (dataModel.directoryTreeModel.size == e.childIndices.length) {
        tree.peer.expandPath(new TreePath(dataModel.directoryTreeModel.peer.getRoot))
      }
  }
}
