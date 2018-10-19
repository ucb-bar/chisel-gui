package visualizer.components

import java.awt.Color

import javax.swing.BorderFactory
import treadle.executable.ClockInfo
import visualizer.models._
import visualizer.{DependencyComponentRequested, MaxTimestampChanged, TreadleController}

import scala.swing.Swing._
import scala.swing._

class MainWindow(dataModel: SelectionController, displayModel: WaveFormController) extends MainFrame {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val signalSelector = new SignalSelector(dataModel, displayModel)
  val inspectionContainer = new InspectionContainer(dataModel, displayModel)
  val dependencyComponent = new DependencyComponent(dataModel, displayModel)
  val inputControlPanel = new InputControlPanel(dataModel, displayModel)

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
    contents += new Menu("File")
  }
  contents = new BorderPanel {
    import BorderPanel.Position._

    preferredSize = (1000, 800)

    focusable = true

    layout(toolbar) = North

    val splitPane: SplitPane = new SplitPane(Orientation.Vertical,
      new ScrollPane(signalSelector) {
        preferredSize = new Dimension(150, 700)
        minimumSize = new Dimension(150, 300)
        border = BorderFactory.createEmptyBorder()
      }, inspectionContainer
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
        dependencyComponent.textComponent.text = TreadleController.tester match {
          case Some(t) => t.dependencyInfo(e.pureSignalName)
          case None => ""
        }
      case e: MaxTimestampChanged =>
        inspectionContainer.zoomToEnd(this)
    }
  }
}
