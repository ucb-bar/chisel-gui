package visualizer.components

import java.io.{File, PrintWriter}

import javax.swing.BorderFactory
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import scalaswingcontrib.tree.Tree
import treadle.executable.ClockInfo
import visualizer.models._
import visualizer.{CursorSet, DependencyComponentRequested, MarkerChanged, MaxTimestampChanged, AppController}

import scala.swing.Swing._
import scala.swing._

/** They main window of the application
  *
  * @param dataModel           Source of data
  * @param selectedSignalModel Source of things selected for waveform view
  */
class MainWindow(dataModel: DataModel, selectionModel: SignalSelectorModel, selectedSignalModel: SelectedSignalModel)
  extends MainFrame {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val signalSelectorPanel = new SignalSelectorPanel(dataModel, selectionModel, selectedSignalModel)
  val signalAndWavePanel = new SignalAndWavePanel(dataModel, selectedSignalModel)
  val showDependenciesPanel = new ShowDependenciesPanel(dataModel, selectedSignalModel)
  val inputControlPanel = new InputControlPanel(dataModel, selectedSignalModel)

  peer.setDefaultCloseOperation(DISPOSE_ON_CLOSE)

  val markerTimeLabel: Label = new Label(s"Cursor: 0 ")

  def setMarkerLabel(time: Long): Unit = {
    markerTimeLabel.text = s"Cursor: $time "
  }

  private val toolbar = new ToolBar() {
    peer.setFloatable(false)

    contents += Button("Zoom In") {
      signalAndWavePanel.zoomIn(this)
    }
    contents += Button("Zoom Out") {
      signalAndWavePanel.zoomOut(this)
    }
    contents += Button("Zoom To End") {
      signalAndWavePanel.zoomToEnd(this)
    }

    contents += HStrut(20)

    contents += markerTimeLabel

    contents += Button("Add Marker") {
      selectedSignalModel.addMarker("ad", selectedSignalModel.cursorPosition)
    }
    contents += Button("Setup mock clock") {
      selectedSignalModel.setClock(ClockInfo("mock clock", 10, 1))
    }
    contents += Button("Toggle Clock") {
      selectedSignalModel.toggleClock()
    }
    contents += Button("Remove signal(s)") {
      signalAndWavePanel.removeSignals(this)
    }
    contents += Button("Add group") {
      selectedSignalModel.addGroup()
    }
  }

  title = "Chisel Visualizer"
  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Save") {
        val chooser = new FileChooser(new File("."))
        val suggestedName = AppController.testerOpt.get.topName + ".save"
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
  }

  override def closeOperation(): Unit = {
    doQuit()
  }

  def doQuit(): Unit = {
    println("Done")

    AppController.testerOpt match {
      case Some(tester) =>
        tester.finish

        val saveFile = new File(AppController.saveFilePrefix + tester.topName + AppController.saveFileSuffix)
        saveSettings(saveFile)
      case _ =>
    }
    this.close()
    super.closeOperation()
    System.exit(0)
  }

  def saveSettings(file: File): Unit = {
    val writer = new PrintWriter(file)

    writer.println(s"window_size,${size.width},${size.height}")

    def walkNodes(path: Tree.Path[GenericTreeNode], depth: Int = 1): Unit = {
      selectedSignalModel.treeModel.getChildPathsOf(path).toArray.zipWithIndex.foreach { case (path, index) =>
        val pathString = path.map { node =>
          node.name
        }.mkString(",")
        val node = path.last
        node match {
          case directoryNode: DirectoryNode =>
            writer.println(s"node,$depth,$index,${directoryNode.name}")
          case waveFormNode: WaveFormNode =>
            waveFormNode.signal match {
              case pureSignal: PureSignal =>
                selectedSignalModel.waveDisplaySettings.get(pureSignal.name) match {
                  case Some(waveDisplaySetting: WaveDisplaySetting) =>
                    val dataFormat = Format.serialize(waveDisplaySetting.dataFormat)
                    writer.println(s"signal_node,$depth,$index,${waveFormNode.name},${pureSignal.name},$dataFormat")
                  case _ =>
                }
              case _ =>
            }
          case _ =>
        }
        walkNodes(path, depth = depth + 1)
      }
    }

    walkNodes(selectedSignalModel.RootPath)

    selectedSignalModel.markers.foreach { marker =>
      writer.println(s"marker,${marker.timestamp}")
    }

    writer.close()
  }

  contents = new BorderPanel {

    import BorderPanel.Position._

    preferredSize = (1000, 800)

    focusable = true

    layout(toolbar) = North

    val splitPane: SplitPane = new SplitPane(
      Orientation.Vertical,
      new ScrollPane(signalSelectorPanel) {
        preferredSize = new Dimension(150, 700)
        minimumSize = new Dimension(150, 300)
        border = BorderFactory.createEmptyBorder()
      },
      signalAndWavePanel
    ) {
      border = BorderFactory.createEmptyBorder()
    }

    layout(splitPane) = Center
    layout(showDependenciesPanel) = South
    layout(inputControlPanel) = East

    listenTo(selectedSignalModel)
    listenTo(dataModel)
    reactions += {
      case e: DependencyComponentRequested =>
        showDependenciesPanel.textComponent.text = AppController.testerOpt match {
          case Some(t) => t.dependencyInfo(e.pureSignalName)
          case None => ""
        }
      case e: CursorSet =>
        setMarkerLabel(selectedSignalModel.cursorPosition)
      case _: MaxTimestampChanged =>
        signalAndWavePanel.zoomToEnd(this)
    }
  }
}
