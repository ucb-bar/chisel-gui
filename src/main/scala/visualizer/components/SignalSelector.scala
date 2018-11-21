package visualizer.components

import javax.swing.tree.TreePath
import javax.swing.{BorderFactory, ImageIcon, SwingUtilities}
import scalaswingcontrib.event.TreeNodesInserted
import scalaswingcontrib.tree.Tree
import visualizer.{SourceInfoRequested, TreadleController}
import visualizer.controllers.{BinFormat, DecFormat, HexFormat, SelectionController}
import visualizer.models._

import scala.collection.mutable
import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyReleased, MouseClicked}

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

    peer.setDragEnabled(true)

    listenTo(mouse.clicks)
    listenTo(keys, keys)

    reactions += {
      case KeyReleased(_, Key.PageDown, _, _) =>
        val pageSize = peer.getVisibleRowCount - 1
        val newSelectedRow = peer.getSelectionRows.lastOption match {
          case Some(row) =>
            (row + pageSize).max(peer.getRowCount - 1)
          case _ =>
            0
        }
        tree.selectRows(newSelectedRow)

        //TODO: Make this work, probably need something like intellij does with a keymap
      case KeyReleased(_, Key.PageUp, _, _) =>
        val pageSize = peer.getVisibleRowCount - 1
        val newSelectedRow = peer.getSelectionRows.lastOption match {
          case Some(row) =>
            (row - pageSize).max(0)
          case _ =>
            peer.getRowCount - 1
        }
        tree.selectRows(newSelectedRow)

      //TODO: Make this work, probably need something like intellij does with a keymap
      case KeyReleased(_, Key.Enter, _, _) | KeyReleased(_, Key.Space, _, _) =>
        moveSelectedToWaveForms()
        peer.getSelectionRows.lastOption match {
          case Some(row) =>
            selectRows(row + 1)
          case _ =>
        }

      //TODO: Make this work, probably need something like intellij does with a keymap
      case KeyReleased(_, Key.End, _, _) =>
        val newSelectedRow = tree.peer.getRowCount - 1
        tree.selectRows(newSelectedRow)

        // Helpful for debugging
      case event @ KeyReleased(_, key, modifiers, _) =>
        println(f"SingalSelector: got key $key mods $modifiers%04x")
        // I don't think this does anything because arrow keys work anyway
//        tree.peer.getParent.dispatchEvent(event.peer)


      case m: MouseClicked =>
        val isRightMouseButton = SwingUtilities.isRightMouseButton(m.peer)

        println(s"SignalSelectorTree: mouse ${if (isRightMouseButton) "right" else "left"} button " +
                s"clicked ${m.clicks} times at position ${m.point}")
        if (m.clicks == 1) {
          if (!isRightMouseButton) {
            //TODO: does the following line allow parent container to pass along this event?
            tree.peer.getParent.dispatchEvent(m.peer)
          }
          else { // Here for right click
            popupMenu().show(tree, m.point.x, m.point.y)
          }
        }
        else if (m.clicks == 2) {
          moveSelectedToWaveForms()
        }
    }
  }

  def moveSelectedToWaveForms(): Unit = {
    val nodesMoved = new mutable.HashSet[SelectionNode]
    var fullPath = Tree.Path.empty[SelectionNode]
    var parentPath = Tree.Path.empty[SelectionNode]

    def addNode(node: SelectionNode): Unit = {
      if (!nodesMoved.contains(node)) {
        displayModel.getPathTo(node.name) match {
          case Some(path) =>
            println(s"path = ${path.mkString("\n")}")
            fullPath = path
            parentPath = path.dropRight(1)
          case None =>
            println(s"Could not find $node to transfer")
            fullPath = selectionController.RootPath
        }

        TreadleController.waveFormController.addHierarchy(parentPath, node)

        node match {
          case group: SelectionGroup =>
            displayModel.getChildrenOf(fullPath).foreach(addNode)
          case signal: SelectionSignal =>
            displayModel.getChildrenOf(fullPath).foreach(addNode)
        }
      }
      nodesMoved += node
    }

    tree.selection.cellValues.foreach(addNode)
  }

  // Popup menu when a signal name is right-clicked
  private def popupMenu(): PopupMenu = new PopupMenu {

    contents += new MenuItem(Action("Add to Waveforms") {
      moveSelectedToWaveForms()
    })

    contents += new MenuItem(Action("Show Source") {
      val symbols = tree.selection.cellValues.collect { case s: SelectionSignal => s }.map(_.symbol).toSeq
      selectionController.showSourceInfo(symbols, tree)
    })

    contents += new MenuItem(Action("Show Dependency Graph") {
      val symbols = tree.selection.cellValues.collect { case s: SelectionSignal => s }.map(_.symbol).toSeq
      TreadleController.waveFormController.showDependency(symbols, this)
    })
  }


  private val toolBar: ToolBar = new ToolBar() {
    peer.setFloatable(false)

//    val r = thisSelector.getClass.getResource("/images/ShowTemps.png")
//    val icon = new ImageIcon(r)
//    val r2 = thisSelector.getClass.getResource("/images/ShowTemps.png")
//    val icon2 = new ImageIcon(r)
////    val tb = new ToggleButton
////    tb.
////    tb.icon = icon
//
//    val toggleButton1 = new Button("Hide _T") {
//      if(text.startsWith("Hide")) { text = "Show _T" }
//      else { text = "Hide _T"}
//    }
//    val toggleButton2 = new Button("Hide _GEN") {
//      if(text.startsWith("Hide")) { text = "Show _GEN" }
//      else { text = "Hide _GEN"}
//    }
//
//    contents += toggleButton1
//    contents += toggleButton2
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
  reactions += {
    case ButtonClicked(`addSymbolsButton`) =>
      moveSelectedToWaveForms()

    case e: TreeNodesInserted[_] =>
      if (selectionController.directoryTreeModel.size == e.childIndices.length) {
        tree.peer.expandPath(new TreePath(selectionController.directoryTreeModel.peer.getRoot))
      }
  }
}
