package visualizer.components

import java.awt.Color

import treadle.executable.ClockInfo
import visualizer.models._
import visualizer.{DependencyComponentRequested, TreadleController}

import scala.swing.Swing._
import scala.swing._

class MainWindow(dataModel: DataModel, displayModel: DisplayModel) extends MainFrame {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val directoryContainer = new DirectoryComponent(dataModel, displayModel)
  val inspectionContainer = new InspectionContainer(dataModel, displayModel)

  private val toolbar = new ToolBar() {
    contents += Button("Zoom In") {
      inspectionContainer.zoomIn(this)
    }
    contents += Button("Zoom Out") {
      inspectionContainer.zoomOut(this)
    }
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

    background = Color.white
    preferredSize = (1000, 800)

    focusable = true

    layout(toolbar) = North

    val splitPane = new SplitPane(Orientation.Vertical,
      new ScrollPane(directoryContainer), inspectionContainer)

    layout(splitPane) = Center

    val dependencyComponent = new DependencyComponent(dataModel, displayModel)
    layout(dependencyComponent) = South

    listenTo(displayModel)
    reactions += {
      case e: DependencyComponentRequested =>
        dependencyComponent.textComponent.text = TreadleController.tester match {
          case Some(t) => t.dependencyInfo(e.pureSignalName)
          case None => ""
        }
    }
  }
}
