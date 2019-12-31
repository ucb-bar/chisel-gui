package visualizer.components

import javax.swing.BorderFactory
import javax.swing.tree.TreePath
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.{InternalTreeModel, Tree}
import visualizer.DrawMetrics
import visualizer.models._

import scala.swing._
import scala.swing.event.{ButtonClicked, EditDone, MouseClicked}

/**
  * Offers all signals in the design to be selected for viewing in
  * wave form viewer.
  * Moves signals to the [[SignalComponent]]
  *
  * @param dataModel    underlying data model
  * @param displayModel underlying displayModel
  */
class SignalSelector(
                      dataModel: DataModel,
                      selectionModel: SelectionModel,
                      displayModel: DisplayModel
                    ) extends BoxPanel(Orientation.Vertical) {

  val me: SignalSelector = this

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val tree: Tree[DirectoryNode] = new Tree[DirectoryNode] {
    model = selectionModel.directoryTreeModel
    renderer = Tree.Renderer(_.name)
    showsRootHandles = true

    listenTo(mouse.clicks)
    reactions += {
      case m: MouseClicked =>
        if (m.clicks == 1) {
          println(s"Got mouse click in tree ${m.clicks}")
        } else if (m.clicks == 2) {
          println(s"mouse double clicked in tree ${m.clicks}")
          selection.cellValues.foreach { node =>
            displayModel.addFromDirectoryToInspected(node.toInspected, this)
          }
        }
    }
  }

  def updateModel(): Unit = {
    tree.model = selectionModel.directoryTreeModel
  }

  class ToggleButton(name: String) extends Button(name) {
    var pushed = false

    def pushAction(thunk: => Unit): Unit = {
      pushed = !pushed
      peer.setForeground(
        if (pushed) {
          DrawMetrics.toggleSelectedBg
        } else {
          DrawMetrics.toggleUnselectedBg
        }
      )
      thunk
    }
  }

  val showTempSignalsButton = new ToggleButton("_T")
  val showGenSignalsButton = new ToggleButton("_Gen")

  val signalPatternText = new TextField("")
  signalPatternText.preferredSize = new Dimension(100, 20)
  signalPatternText.peer.setMaximumSize(signalPatternText.peer.getPreferredSize)

  private val toolBar = new ToolBar() {
    peer.setFloatable(false)

    contents += showTempSignalsButton
    contents += showGenSignalsButton
    contents += signalPatternText
  }

  contents += toolBar

  val addSymbolsButton = new Button("Add")
  val symbolList: ScrollPane = new ScrollPane(tree) {
    border = BorderFactory.createEmptyBorder()
  }
  contents += symbolList

  private val lowerToolbar = new FlowPanel {
    contents += addSymbolsButton
  }

  contents += lowerToolbar

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(addSymbolsButton)
  listenTo(showTempSignalsButton)
  listenTo(showGenSignalsButton)
  listenTo(addSymbolsButton)
  listenTo(signalPatternText)
  listenTo(tree)
  listenTo(mouse.clicks)

  reactions += {
    //TODO remove commented code, appears to be wrestling with mouse click listener above
    //    case m: MouseClicked =>
    //      if (m.clicks == 1) {
    //        println(s"Got mouse click in DirectoryComponent ${m.clicks}")
    //      } else if (m.clicks == 2) {
    //        println(s"mouse double clicked in DirectoryComponent ${m.clicks}")
    //        tree.selection.cellValues.foreach { node =>
    //          displayModel.addFromDirectoryToInspected(node.toInspected, this)
    //        }
    //      }

    case ButtonClicked(`addSymbolsButton`) =>
      tree.selection.cellValues.foreach { node =>
        displayModel.addFromDirectoryToInspected(node.toInspected, this)
      }

    case ButtonClicked(`showTempSignalsButton`) =>
      showTempSignalsButton.pushAction {
        selectionModel.dataModelFilter = selectionModel.dataModelFilter.copy(
          showTempVariables = showTempSignalsButton.pushed
        )
        selectionModel.updateTreeModel()
        tree.model = selectionModel.directoryTreeModel
      }

    case ButtonClicked(`showGenSignalsButton`) =>
      showGenSignalsButton.pushAction {
        selectionModel.dataModelFilter = selectionModel.dataModelFilter.copy(
          showGenVariables = showGenSignalsButton.pushed
        )
        selectionModel.updateTreeModel()
        tree.model = selectionModel.directoryTreeModel
      }

    case EditDone(`signalPatternText`) =>
      selectionModel.dataModelFilter = selectionModel.dataModelFilter.copy(
        pattern = signalPatternText.text
      )
      selectionModel.updateTreeModel()
      tree.model = selectionModel.directoryTreeModel

    case e: TreeNodesInserted[_] =>
      if (selectionModel.directoryTreeModel.size == e.childIndices.length) {
        tree.peer.expandPath(new TreePath(selectionModel.directoryTreeModel.peer.getRoot))
      }
  }
}
