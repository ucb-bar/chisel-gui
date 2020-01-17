package visualizer.components

import javax.swing.tree.TreePath
import javax.swing.{BorderFactory, SwingUtilities}
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.Tree
import scalaswingcontrib.tree.Tree.Path
import visualizer.config.DrawMetrics
import visualizer.models._
import visualizer.{ChiselGUI, SignalsChanged}

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
  dataModel:           DataModel,
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

    requestFocus()

    listenTo(mouse.clicks)
    listenTo(keys)

    reactions += {
      case KeyReleased(_, key, modifiers, _) =>
        if (key == Key.Enter) {
          addToSelectedSignalsModel(InsertAfter)
        } else if (key == Key.Right && (modifiers & Key.Modifier.Shift) > 0) {

          ChiselGUI.mainWindow.signalAndWavePanel.tree.requestFocus()
          ChiselGUI.mainWindow.signalAndWavePanel.tree.requestFocusInWindow()
        }
      case m: MouseClicked =>
        if (SwingUtilities.isLeftMouseButton(m.peer)) {
          if (m.clicks == 1) {
            println(s"Got mouse click in tree ${m.clicks}")
          } else if (m.clicks == 2) {
            println(s"mouse double clicked in tree ${m.clicks}")
            selection.cellValues.foreach {
              case directoryNode: DirectoryNode =>
                selectedSignalModel.addFromDirectoryToInspected(directoryNode.copy(), this)
              case otherNode =>
                selectedSignalModel.addFromDirectoryToInspected(otherNode, this)
            }
          }
        }
    }
  }

  def updateModel(): Unit = {
    tree.model = signalSelectorModel.directoryTreeModel
  }

  /** Add the selected fields to the SelectedSignalsPanel
    * Use the first selected value of the SelectedSignalsPanel as the insert point
    *
    * @param addDirection Where to put
    */
  def addToSelectedSignalsModel(addDirection: AddDirection): Unit = {

    def addPath(path:          Path[GenericTreeNode],
                addDirection:  AddDirection,
                targetPathOpt: Option[Path[GenericTreeNode]] = None) {
      path.last match {
        case directoryNode: DirectoryNode =>
          if (tree.isExpanded(path)) {
            selectedSignalModel.addNodes(addDirection, directoryNode, source = this, targetPathOpt)
          } else {
            val lastTarget = selectedSignalModel.addNodes(addDirection, directoryNode, source = this, targetPathOpt)
            val childPaths = tree.model.getChildPathsOf(path).toArray
            childPaths.foreach { child_path =>
              addPath(child_path, InsertInto, Some(lastTarget))
            }
          }
        case otherNode =>
          if (tree.isExpanded(path)) {
            selectedSignalModel.addNodes(addDirection, otherNode, source = this, targetPathOpt)
          } else {
            val lastTarget = selectedSignalModel.addNodes(addDirection, otherNode, source = this, targetPathOpt)
            val childPaths = tree.model.getChildPathsOf(path).toArray
            childPaths.foreach { child_path =>
              addPath(child_path, InsertInto, Some(lastTarget))
            }
          }
      }
    }

    val paths = tree.selection.paths.toArray
    paths.foreach { path =>
      addPath(path, addDirection)
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

  val insertSignalBeforeButton = new Button("↑")
  val insertSignalAfterButton = new Button("↓")
  val insertSignalIntoButton = new Button("→")
  val appendSignalButton = new Button("⤓")

  val symbolList: ScrollPane = new ScrollPane(tree) {
    border = BorderFactory.createEmptyBorder()
  }

  contents += symbolList

  contents += Swing.Glue

  private val lowerToolbar = new ToolBar {
    peer.setFloatable(false)

    contents += new Label("Insert:")
    contents += insertSignalBeforeButton
    contents += insertSignalAfterButton
    contents += insertSignalIntoButton
    contents += appendSignalButton
    // contents += Swing.Glue
  }

  contents += lowerToolbar

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////

  def refreshSelected(): Unit = {
    publish(SignalsChanged(this))
  }

  listenTo(insertSignalBeforeButton)
  listenTo(insertSignalIntoButton)
  listenTo(insertSignalAfterButton)
  listenTo(appendSignalButton)

  listenTo(showTempSignalsButton)
  listenTo(showGenSignalsButton)
  listenTo(appendSignalButton)
  listenTo(signalPatternText)
  listenTo(tree)

  reactions += {
    case ButtonClicked(`insertSignalBeforeButton`) =>
      addToSelectedSignalsModel(InsertBefore)

    case ButtonClicked(`insertSignalIntoButton`) =>
      addToSelectedSignalsModel(InsertInto)

    case ButtonClicked(`insertSignalAfterButton`) =>
      addToSelectedSignalsModel(InsertAfter)

    case ButtonClicked(`appendSignalButton`) =>
      addToSelectedSignalsModel(AppendToEnd)

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
}
