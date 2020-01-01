package visualizer.components

import java.io.{File, PrintWriter}

import javax.swing.BorderFactory
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import treadle.executable.ClockInfo
import visualizer.models._
import visualizer.{DependencyComponentRequested, MaxTimestampChanged, TreadleController}

import scala.swing.Swing._
import scala.swing._

/** They main window of the application
  *
  * @param dataModel    Source of data
  * @param displayModel Source of things selected for waveform view
  */
class MainWindow(dataModel: DataModel, selectionModel: SelectionModel, displayModel: DisplayModel) extends MainFrame {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val signalSelector = new SignalSelector(dataModel, selectionModel, displayModel)
  val inspectionContainer = new InspectionContainer(dataModel, displayModel)
  val dependencyComponent = new DependencyComponent(dataModel, displayModel)
  val inputControlPanel = new InputControlPanel(dataModel, displayModel)

  peer.setDefaultCloseOperation(DISPOSE_ON_CLOSE)

  private val toolbar = new ToolBar() {
    peer.setFloatable(false)

    contents += Button("Zoom In") {
      inspectionContainer.zoomIn(this)
    }
    contents += Button("Zoom Out") {
      inspectionContainer.zoomOut(this)
    }
    contents += Button("Zoom To End") {
      inspectionContainer.zoomToEnd(this)
    }

    contents += HStrut(20)

    contents += Button("Add Marker") {
      displayModel.addMarker("ad", displayModel.cursorPosition)
    }
    contents += Button("Setup mock clock") {
      displayModel.setClock(ClockInfo("mock clock", 10, 1))
    }
    contents += Button("Toggle Clock") {
      displayModel.toggleClock()
    }
    contents += Button("Remove signal(s)") {
      inspectionContainer.removeSignals(this)
    }
    contents += Button("Add group") {
      displayModel.addGroup()
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
  override def closeOperation(): Unit = doQuit()

  def doQuit(): Unit = {
    println("Done")

    TreadleController.testerOpt match {
      case Some(tester) =>
        tester.finish
      case _ =>
    }
    this.close()
    System.exit(0)
  }

  def saveSettings(file: File): Unit = {
    val writer = new PrintWriter(file)

    inspectionContainer.tree.cellValues.foreach { node =>
      val dataFormat = displayModel.waveDisplaySettings(node.nodeId).dataFormat match {
        case Some(BinFormat) => "bin"
        case Some(HexFormat) => "hex"
        case Some(DecFormat) => "dec"
        case _ => "none"
      }
      writer.println(s"node,${node.name},$dataFormat")
    }

    displayModel.markers.foreach { marker =>
      writer.println(s"maker,${marker.timestamp}")
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
      new ScrollPane(signalSelector) {
        preferredSize = new Dimension(150, 700)
        minimumSize = new Dimension(150, 300)
        border = BorderFactory.createEmptyBorder()
      },
      inspectionContainer
    ) {
      border = BorderFactory.createEmptyBorder()
    }

    layout(splitPane) = Center
    layout(dependencyComponent) = South
    layout(inputControlPanel) = East

    listenTo(displayModel)
    listenTo(dataModel)
    reactions += {
      case e: DependencyComponentRequested =>
        dependencyComponent.textComponent.text = TreadleController.testerOpt match {
          case Some(t) => t.dependencyInfo(e.pureSignalName)
          case None => ""
        }
      case e: MaxTimestampChanged =>
        inspectionContainer.zoomToEnd(this)
    }
  }
}
