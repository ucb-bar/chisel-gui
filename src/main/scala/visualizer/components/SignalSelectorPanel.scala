package visualizer.components

import javax.swing.BorderFactory
import javax.swing.tree.TreePath
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.Tree
import visualizer.DrawMetrics
import visualizer.models._

import scala.swing._
import scala.swing.event._

/**
  * Offers all signals in the design to be selected for viewing in
  * wave form viewer.
  * Moves signals to the [[SelectedSignalPanel]]
  *
  * @param dataModel           underlying data model
  * @param selectedSignalModel underlying model for signals that have been selected for viewing
  */
class SignalSelectorPanel(
                           dataModel: DataModel,
                           signalSelectorModel: SignalSelectorModel,
                           selectedSignalModel: SelectedSignalModel
                         ) extends BoxPanel(Orientation.Vertical) {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val tree: Tree[GenericTreeNode] = new Tree[GenericTreeNode] {
    model = signalSelectorModel.directoryTreeModel
    renderer = Tree.Renderer(_.name)
    showsRootHandles = true
    dragEnabled = true

    listenTo(mouse.clicks)
    listenTo(keys)

    reactions += {
      case KeyReleased(_, key, _, _) =>
        if (key == Key.Enter) {
          addSelectedToInspection(append = false)
        }
      case m: MouseClicked =>
        if (m.clicks == 1) {
          println(s"Got mouse click in tree ${m.clicks}")
        } else if (m.clicks == 2) {
          println(s"mouse double clicked in tree ${m.clicks}")
          selection.cellValues.foreach {
            case directoryNode: DirectoryNode =>
              //TODO: This will not bring along the children, should it?
              selectedSignalModel.addFromDirectoryToInspected(directoryNode.copy(), this)
            case otherNode =>
              selectedSignalModel.addFromDirectoryToInspected(otherNode, this)
          }
        }
    }
  }

  def updateModel(): Unit = {
    tree.model = signalSelectorModel.directoryTreeModel
  }

  def addSelectedToInspection(append: Boolean): Unit = {
    tree.selection.cellValues.foreach {
      case directoryNode: DirectoryNode =>
        selectedSignalModel.addFromDirectoryToInspected(directoryNode.copy(), this)
      case otherNode =>
        selectedSignalModel.addFromDirectoryToInspected(otherNode, this)
    }
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

  val insertSymbolsButton = new Button("Insert")
  val appendSymbolsButton = new Button("Append")

  val symbolList: ScrollPane = new ScrollPane(tree) {
    border = BorderFactory.createEmptyBorder()
  }

  contents += symbolList

  contents += Swing.Glue

  private val lowerToolbar = new ToolBar {
    peer.setFloatable(false)

    contents += insertSymbolsButton
    contents += appendSymbolsButton
    // contents += Swing.Glue
  }

  contents += lowerToolbar

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(appendSymbolsButton)
  listenTo(showTempSignalsButton)
  listenTo(showGenSignalsButton)
  listenTo(appendSymbolsButton)
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
    //          selectedSignalModel.addFromDirectoryToInspected(node.toInspected, this)
    //        }
    //      }

    case ButtonClicked(`appendSymbolsButton`) =>
      addSelectedToInspection(append = true)

    case ButtonClicked(`insertSymbolsButton`) =>
      addSelectedToInspection(append = false)

    case ButtonClicked(`showTempSignalsButton`) =>
      showTempSignalsButton.pushAction {
        signalSelectorModel.dataModelFilter = signalSelectorModel.dataModelFilter.copy(
          showTempVariables = showTempSignalsButton.pushed
        )
        signalSelectorModel.updateTreeModel()
        tree.model = signalSelectorModel.directoryTreeModel
      }

    case ButtonClicked(`showGenSignalsButton`) =>
      showGenSignalsButton.pushAction {
        signalSelectorModel.dataModelFilter = signalSelectorModel.dataModelFilter.copy(
          showGenVariables = showGenSignalsButton.pushed
        )
        signalSelectorModel.updateTreeModel()
        tree.model = signalSelectorModel.directoryTreeModel
      }

    case EditDone(`signalPatternText`) =>
      signalSelectorModel.dataModelFilter = signalSelectorModel.dataModelFilter.copy(
        pattern = signalPatternText.text
      )
      signalSelectorModel.updateTreeModel()
      tree.model = signalSelectorModel.directoryTreeModel

    case e: TreeNodesInserted[_] =>
      if (signalSelectorModel.directoryTreeModel.size == e.childIndices.length) {
        tree.peer.expandPath(new TreePath(signalSelectorModel.directoryTreeModel.peer.getRoot))
      }
  }

  focusable = true
  requestFocus()
}
