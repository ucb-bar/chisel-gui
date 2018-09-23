package visualizer.components

import javax.swing.BorderFactory
import javax.swing.tree.TreePath
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.Tree
import visualizer.models._

import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked}

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

    listenTo(mouse.clicks)
    reactions += {
      case m: MouseClicked =>
        if(m.clicks == 1) {
          println(s"Got mouse click in tree ${m.clicks}")
        }
        else if(m.clicks == 2) {
          println(s"mouse double clicked in tree ${m.clicks}")
          selection.cellValues.foreach { node =>
            displayModel.addFromDirectoryToInspected(node.toInspected, this)
          }
        }
    }
  }

  val addSymbolsButton = new Button("Add")
  val symbolList: ScrollPane = new ScrollPane(tree) {
    border = BorderFactory.createEmptyBorder()
  }
  contents += symbolList
  contents += addSymbolsButton

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(addSymbolsButton)
  listenTo(tree)
  listenTo(mouse.clicks)
  reactions += {
    case m: MouseClicked =>
      if(m.clicks == 1) {
        println(s"Got mouse click in DirectoryComponent ${m.clicks}")
      }
      else if(m.clicks == 2) {
        println(s"mouse double clicked in DirectoryComponent ${m.clicks}")
        tree.selection.cellValues.foreach { node =>
          displayModel.addFromDirectoryToInspected(node.toInspected, this)
        }
      }
    case ButtonClicked(`addSymbolsButton`) =>
      tree.selection.cellValues.foreach{node =>
        displayModel.addFromDirectoryToInspected(node.toInspected, this)
      }
    case e: TreeNodesInserted[_] =>
      if (dataModel.directoryTreeModel.size == e.childIndices.length) {
        tree.peer.expandPath(new TreePath(dataModel.directoryTreeModel.peer.getRoot))
      }
  }
}
