package visualizer.components

import java.io.{File, PrintWriter}

import javax.swing.BorderFactory
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import scalaswingcontrib.tree.Tree
import visualizer.config.ColorTable
import visualizer.models._
import visualizer.{ChiselGUI, CursorSet, DependencyComponentRequested, MaxTimestampChanged}

import scala.swing.Swing._
import scala.swing._

/** They main window of the application
  *
  * @param dataModel           Source of data
  * @param selectedSignalModel Source of things selected for waveform view
  */
class MainWindow(dataModel: DataModel, selectionModel: SignalSelectorModel, selectedSignalModel: SelectedSignalModel)
    extends MainFrame {

  val mainWindowRef: MainWindow = this

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val signalSelectorPanel = new SignalSelectorPanel(dataModel, selectionModel, selectedSignalModel)
  val signalAndWavePanel = new SignalAndWavePanel(dataModel, selectedSignalModel)
  val showDependenciesPanel = new ShowDependenciesPanel(dataModel, selectedSignalModel)
  val inputControlPanel = new InputControlPanel(dataModel, selectedSignalModel)

  peer.setDefaultCloseOperation(DISPOSE_ON_CLOSE)

  val markerCursorLabel: Label = new Label(s"Cursor: 0 ")

  def setMarkerLabel(time: Long): Unit = {
    markerCursorLabel.text = s"Cursor: $time "
  }

  private val toolbar = new ToolBar() {
    peer.setFloatable(false)

    contents += HStrut(300)

    contents += new Label("Zoom")

    contents += Button("⇤") {
      signalAndWavePanel.zoomToStart(this)
    }
    contents += Button("⇥ ⇤") {
      signalAndWavePanel.zoomIn(this)
    }
    contents += Button("⇤ ⇥") {
      signalAndWavePanel.zoomOut(this)
    }
    contents += Button("⇥") {
      signalAndWavePanel.zoomToEnd(this)
    }

    contents += HStrut(20)

    contents += Button("Add Marker") {
      createMarker()
    }
    contents += Button("Toggle Ruler") {
      selectedSignalModel.toggleClock()
    }
    contents += Button("Add group") {
      Dialog.showInput(
        this,
        "GroupName: ",
        title = "Add New Group",
        initial = "NewGroup"
      ) match {
        case Some(newGroupName) =>
          selectedSignalModel.addGroup(newGroupName)
        case _ =>
      }
    }
  }

  private val statusBar = new ToolBar() {
    peer.setFloatable(false)

    contents += HStrut(400)

    contents += markerCursorLabel
  }

  title = "Chisel Visualizer"
  var isAltColorScheme = false
  var inputPanelVisible = false

  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Save") {
        val chooser = new FileChooser(new File("."))
        val suggestedName = ChiselGUI.testerOpt.get.topName + ".save"
        chooser.selectedFile = new File(suggestedName)

        val result = chooser.showSaveDialog(this)
        if (result == FileChooser.Result.Approve) {
          val saveFile = chooser.selectedFile
          saveSettings(saveFile)
        }
      })
      contents += new Separator()
      contents += new MenuItem(Action("Quit") {
        doQuit()
      })
    }
    contents += new Menu("Edit") {
      contents += new MenuItem("") {
        action = Action("Add Marker") {
          createMarker()
        }
      }
      contents += new MenuItem("") {
        action = Action("Remove Marker") {
          destroyMarker()
        }
      }
    }
    contents += new Menu("View") {
      contents += new MenuItem("") {
        action = Action("Show Signal Selector") {
          waveAndSignalContainer.signalSelectorContainer.visible =
            !waveAndSignalContainer.signalSelectorContainer.visible
        }
      }
      contents += new MenuItem("") {
        action = Action("Show Input Panel") {
          inputControlPanel.visible = !inputControlPanel.visible
          //          signalAndWavePanel.updateWaveView()
        }
      }

      contents += VStrut(20)

      contents += new MenuItem("") {
        action = Action("Toggle Wave Colors") {
          isAltColorScheme = !isAltColorScheme
          if (isAltColorScheme) {
            ColorTable.setAltWaveColors()
          } else {
            ColorTable.setDefaultWaveColors()
          }
          signalAndWavePanel.updateWaveView()
        }
      }
      contents += new MenuItem("") {
        action = Action("Toggle Aggregating decoupled bundles") {
          ChiselGUI.signalSelectorModel.setRollupDecoupled(
            !ChiselGUI.signalSelectorModel.dataModelFilter.rollupDecoupled
          )
          ChiselGUI.signalSelectorModel.updateTreeModel()
          ChiselGUI.mainWindow.signalSelectorPanel.tree.model = ChiselGUI.signalSelectorModel.directoryTreeModel
        }
      }
    }
    contents += new Menu("Help") {
      contents += new MenuItem(Action("Show Version") {
        Dialog.showMessage(this, "Version 0.4 01/12/2020")
      })
    }
  }

  override def closeOperation(): Unit = {
    doQuit()
  }

  def doQuit(): Unit = {
    println("Done")

    ChiselGUI.testerOpt match {
      case Some(tester) =>
        tester.finish

        val saveFile = new File(ChiselGUI.saveFilePrefix + tester.topName + ChiselGUI.saveFileSuffix)
        saveSettings(saveFile)
      case _ =>
    }
    this.close()
    super.closeOperation()
    System.exit(0)
  }

  def createMarker(): Unit = {
    Dialog.showInput(
      this,
      "MarkerName: ",
      title = "Add a time marker",
      initial = s"Marker_${selectedSignalModel.cursorPosition}"
    ) match {
      case Some(newMarkerName) =>
        selectedSignalModel.addMarker(newMarkerName, selectedSignalModel.cursorPosition)
        signalAndWavePanel.updateWaveView()
      case _ =>
    }
  }

  def destroyMarker(): Unit = {
    Dialog.showInput(
      this,
      "Remove Marker: ",
      title = "Remove a marker by name",
      initial = s""
    ) match {
      case Some(newMarkerName) =>
        selectedSignalModel.removeMarker(newMarkerName)
        signalAndWavePanel.updateWaveView()
      case _ =>
    }
  }

  def saveSettings(file: File): Unit = {
    val writer = new PrintWriter(file)

    writer.println(s"window_size,${size.width},${size.height}")

    def walkNodes(path: Tree.Path[GenericTreeNode], depth: Int = 1): Unit = {
      selectedSignalModel.treeModel.getChildPathsOf(path).toArray.zipWithIndex.foreach {
        case (path, index) =>
          val expand = if (signalAndWavePanel.tree.isExpanded(path)) "expand" else "close"

          val node = path.last
          node match {
            case directoryNode: DirectoryNode =>
              writer.println(s"node,$depth,$index,${directoryNode.name},$expand")
            case waveFormNode: WaveFormNode =>
              waveFormNode.signal match {
                case pureSignal: PureSignal =>
                  selectedSignalModel.waveDisplaySettings.get(pureSignal.name) match {
                    case Some(waveDisplaySetting: WaveDisplaySetting) =>
                      val dataFormat = Format.serialize(waveDisplaySetting.dataFormat)
                      writer.println(
                        s"signal_node,$depth,$index,${waveFormNode.name},${pureSignal.name},$dataFormat,$expand"
                      )
                    case _ =>
                  }
                case decoupledSignalGroup: DecoupledSignalGroup =>
                  selectedSignalModel.waveDisplaySettings.get(decoupledSignalGroup.name) match {
                    case Some(waveDisplaySetting: WaveDisplaySetting) =>
                      val dataFormat = Format.serialize(waveDisplaySetting.dataFormat)
                      writer.println(
                        s"decoupled_node,$depth,$index,${waveFormNode.name}," +
                          s"${decoupledSignalGroup.name},$dataFormat,$expand"
                      )
                    case _ =>
                      writer.println(
                        s"decoupled_node,$depth,$index,${waveFormNode.name},${decoupledSignalGroup.name},none,$expand"
                      )
                  }
                case _ =>
              }
            case _ =>
          }
          walkNodes(path, depth = depth + 1)
      }
    }

    walkNodes(selectedSignalModel.RootPath)

    writer.println(s"cursor-position,${selectedSignalModel.cursorPosition}")

    selectedSignalModel.markers.foreach { marker =>
      writer.println(s"marker,${marker.description},${marker.timestamp},")
    }

    val waveColorCode = if (ChiselGUI.mainWindow.isAltColorScheme) "alt" else "default"
    writer.println(s"wave-colors,$waveColorCode")

    val visibleRect = signalAndWavePanel.wavePanel.peer.getVisibleRect
    writer.println(s"scale-and-window,${selectedSignalModel.scale},${visibleRect.x}")

    writer.println(s"aggregate_decoupled,${selectionModel.dataModelFilter.rollupDecoupled.toString}")

    writer.close()
  }

  val waveAndSignalContainer = new BorderPanel {

    import BorderPanel.Position._

    preferredSize = (1000, 800)

    focusable = true

    layout(toolbar) = North

    val signalSelectorContainer: ScrollPane = new ScrollPane(signalSelectorPanel) {
      preferredSize = new Dimension(150, 700)
      minimumSize = new Dimension(150, 300)
      border = BorderFactory.createEmptyBorder()
    }

    val splitPane: SplitPane = new SplitPane(
      Orientation.Vertical,
      signalSelectorContainer,
      signalAndWavePanel
    ) {
      border = BorderFactory.createEmptyBorder()
    }

    layout(splitPane) = Center
    layout(statusBar) = South
    layout(inputControlPanel) = East

    listenTo(selectedSignalModel)
    listenTo(dataModel)
    listenTo(mainWindowRef)

    reactions += {
      case e: DependencyComponentRequested =>
        showDependenciesPanel.textComponent.text = ChiselGUI.testerOpt match {
          case Some(t) => t.dependencyInfo(e.pureSignalName)
          case None => ""
        }
      case e: CursorSet =>
        setMarkerLabel(selectedSignalModel.cursorPosition)
      case _: MaxTimestampChanged =>
        signalAndWavePanel.zoomToEnd(this)
    }
  }
  contents = waveAndSignalContainer
}
