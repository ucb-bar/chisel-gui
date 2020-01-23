package visualizer.components

import java.io.{File, PrintWriter}

import javax.swing.{BorderFactory, SwingUtilities}
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import scalaswingcontrib.tree.Tree
import visualizer.config.ColorTable
import visualizer.models._
import visualizer.{ChiselGUI, CursorSet, DependencyComponentRequested, MaxTimestampChanged, PanelsChanged}

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

  // import java.awt.Desktop
  // This is supposed to work on Java 9+
  //  val desktop = Desktop.getDesktop()
  //  desktop.setAboutHandler(e ->
  //    JOptionPane.showMessageDialog(null, "About dialog")
  //  );
  //  desktop.setPreferencesHandler(e ->
  //    JOptionPane.showMessageDialog(null, "Preferences dialog")
  //  );
  //  desktop.setQuitHandler((e,r) -> {
  //    JOptionPane.showMessageDialog(null, "Quit dialog");
  //    System.exit(0);
  //  }
  //  );

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

    val zoomToStart = Button("⇤") {
      signalAndWavePanel.zoomToStart(this)
    }
    zoomToStart.tooltip = "Zoom to Start"
    contents += zoomToStart
    val zoomIn = Button("⇥ ⇤") {
      signalAndWavePanel.zoomIn(this)
    }
    zoomIn.tooltip = "Zoom In"
    contents += zoomIn
    val zoomOut = Button("⇤ ⇥") {
      signalAndWavePanel.zoomOut(this)
    }
    zoomOut.tooltip = "Zoom Out"
    contents += zoomOut
    val zoomEnd = Button("⇥") {
      signalAndWavePanel.zoomToEnd(this)
    }
    zoomEnd.tooltip = "Zoom to End"
    contents += zoomEnd

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
  var saveSplitPaneDividerLocation: Int = 150

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
      contents += new CheckMenuItem("") {
        selected = ChiselGUI.startupShowSignalSelector
        action = Action("Show Signal Selector") {
          toggleSignalSelector()
          pingSplitPane()
        }
      }

      if (ChiselGUI.testerOpt.isDefined) {
        contents += new CheckMenuItem("") {
          selected = true
          action = Action("Show Input Panel") {
            inputControlPanel.visible = !inputControlPanel.visible
          }
        }
      }

      contents += VStrut(20)

      contents += new CheckMenuItem("") {
        selected = ChiselGUI.startUpColorScheme != "default"
        action = Action("Use Dark Wave Colors") {
          isAltColorScheme = !isAltColorScheme
          if (isAltColorScheme) {
            ColorTable.setAltWaveColors()
          } else {
            ColorTable.setDefaultWaveColors()
          }
          signalAndWavePanel.updateWaveView()
        }
      }
      contents += new CheckMenuItem("") {
        this.selected = ChiselGUI.startupAggregateDecoupledFlag
        action = Action("RollUp Decoupled Bundles") {
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
                case validSignalGroup: ValidSignalGroup =>
                  selectedSignalModel.waveDisplaySettings.get(validSignalGroup.name) match {
                    case Some(waveDisplaySetting: WaveDisplaySetting) =>
                      val dataFormat = Format.serialize(waveDisplaySetting.dataFormat)
                      writer.println(
                        s"valid_node,$depth,$index,${waveFormNode.name}," +
                          s"${validSignalGroup.name},$dataFormat,$expand"
                      )
                    case _ =>
                      writer.println(
                        s"valid_node,$depth,$index,${waveFormNode.name},${validSignalGroup.name},none,$expand"
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

    writer.println(s"show_signal_selector,$isSignalSelectorVisible")

    writer.close()
  }

  val mainContainer = new BorderPanel {

    import BorderPanel.Position._

    preferredSize = (1000, 800)

    focusable = true

    layout(toolbar) = North

    val signalSelectorContainer: ScrollPane = new ScrollPane(signalSelectorPanel) {
      preferredSize = new Dimension(150, 700)
      minimumSize = new Dimension(150, 300)
      border = BorderFactory.createEmptyBorder()
      visible = ChiselGUI.startupShowSignalSelector
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
  contents = mainContainer

  System.setProperty("com.apple.mrj.application.apple.menu.about.name", "ChiselGUI")

  def toggleSignalSelector(): Unit = {
    mainContainer.signalSelectorContainer.visible = if (mainContainer.signalSelectorContainer.visible) {
      saveSplitPaneDividerLocation = mainContainer.splitPane.dividerLocation
      false
    } else {
      mainContainer.signalSelectorContainer.visible = !mainContainer.signalSelectorContainer.visible
      mainContainer.splitPane.dividerLocation = saveSplitPaneDividerLocation
      true
    }
  }

  def isSignalSelectorVisible: Boolean = mainContainer.signalSelectorContainer.visible

  def pingSplitPane(): Unit = {
    // The blasted remains of shotgun technique to get SignalSelectorPanel to re-display after hiding
    SwingUtilities.invokeLater(() => {
      if (mainContainer.signalSelectorContainer.visible) {
        mainContainer.splitPane.peer.revalidate()
        mainContainer.splitPane.peer.repaint()
      } else {
        //        mainContainer.signalSelectorContainer.peer.setSize(new Dimension(150,800))
        signalSelectorPanel.peer.revalidate()
        signalSelectorPanel.peer.repaint()
        //        mainContainer.signalSelectorContainer.peer.revalidate()
        //        mainContainer.signalSelectorContainer.peer.repaint()
      }
      mainContainer.peer.revalidate()
      mainContainer.repaint()
    })
  }
}
