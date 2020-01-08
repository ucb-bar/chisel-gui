package visualizer.components

import java.io.{File, PrintWriter}

import javax.swing.BorderFactory
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import treadle.executable.ClockInfo
import visualizer.models._
import visualizer.{CursorSet, DependencyComponentRequested, MarkerChanged, MaxTimestampChanged, TreadleController}

import scala.swing.Swing._
import scala.swing._

/** They main window of the application
  *
  * @param dataModel           Source of data
  * @param selectedSignalModel Source of things selected for waveform view
  */
class MainWindow(dataModel: DataModel, selectionModel: SignalSelectorModel, selectedSignalModel: SelectedSignalModel) extends MainFrame {

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
        val suggestedName = TreadleController.testerOpt.get.topName + ".save"
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

  //TODO this does not seem to handle Command-Q as was hoped
  override def closeOperation(): Unit = {
    doQuit()
  }

  def doQuit(): Unit = {
    println("Done")

    TreadleController.testerOpt match {
      case Some(tester) =>
        tester.finish
      case _ =>
    }
    this.close()
    super.closeOperation()
    System.exit(0)
  }

  def saveSettings(file: File): Unit = {
    val writer = new PrintWriter(file)

    writer.println(s"windowsize,${size.width},${size.height}")
    signalAndWavePanel.tree.cellValues.foreach {
      case waveFormNode: WaveFormNode =>
        waveFormNode.signal match {
          case _: PureSignal =>
            selectedSignalModel.waveDisplaySettings.get(waveFormNode.name) match {
              case Some(waveDisplaySetting: WaveDisplaySetting) =>
                val dataFormat = Format.serialize(waveDisplaySetting.dataFormat)
                writer.println(s"node,${waveFormNode.name},$dataFormat")
              case _ =>
            }
          case _ =>
        }
    }

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
        showDependenciesPanel.textComponent.text = TreadleController.testerOpt match {
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
