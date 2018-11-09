package visualizer.components

import javax.swing.tree.TreePath
import javax.swing.{BorderFactory, ImageIcon}
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.Tree
import visualizer.controllers.SelectionController
import visualizer.models._

import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked}

/**
  * Offers all signals in the design to be selected for viewing in wave form viewer
  * @param selectionController    underlying data model
  */
class SignalSelector(selectionController: SelectionController) extends BoxPanel(Orientation.Vertical) {

  val displayModel: SignalSelectionModel = selectionController.directoryTreeModel
  val thisSelector: SignalSelector = this

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val tree: Tree[SelectionNode] = new Tree[SelectionNode] {
    model = displayModel
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
            selectionController.addToWaveFormViewer(node)
          }
        }
    }
  }

  private val toolBar: ToolBar = new ToolBar() {
    peer.setFloatable(false)

    val r = thisSelector.getClass.getResource("/images/ShowTemps.png")
    val icon = new ImageIcon(r)
    val r2 = thisSelector.getClass.getResource("/images/ShowTemps.png")
    val icon2 = new ImageIcon(r)
//    val tb = new ToggleButton
//    tb.
//    tb.icon = icon

    val toggleButton1 = new Button("Hide _T") {
      if(text.startsWith("Hide")) { text = "Show _T" }
      else { text = "Hide _T"}
    }
    val toggleButton2 = new Button("Hide _GEN") {
      if(text.startsWith("Hide")) { text = "Show _GEN" }
      else { text = "Hide _GEN"}
    }

    contents += toggleButton1
    contents += toggleButton2
  }

  contents += toolBar

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
          selectionController.addToWaveFormViewer(node)
        }
      }
    case ButtonClicked(`addSymbolsButton`) =>
      tree.selection.cellValues.foreach{node =>
        selectionController.addToWaveFormViewer(node)
      }
    case e: TreeNodesInserted[_] =>
      if (selectionController.directoryTreeModel.size == e.childIndices.length) {
        tree.peer.expandPath(new TreePath(selectionController.directoryTreeModel.peer.getRoot))
      }
  }
}
