package visualizer.components

import javax.swing.BorderFactory
import treadle.executable.ClockInfo
import visualizer.controllers.{SelectionController, WaveFormController}
import visualizer.{DependencyComponentRequested, MaxTimestampChanged, SourceInfoRequested, TreadleController}

import scala.swing.Swing._
import scala.swing._

class MainWindow(selectionController: SelectionController, waveFormController: WaveFormController) extends MainFrame {

  ///////////////////////////////////////////////////////////////////////////
  // Views
  ///////////////////////////////////////////////////////////////////////////
  val signalSelector: SignalSelector           = selectionController.signalSelector
  val inspectionContainer: InspectionContainer = waveFormController.inspectionContainer

  val dependencyComponent: DependencyComponent = new DependencyComponent(selectionController)
  val inputControlPanel: InputControlPanel     = new InputControlPanel(waveFormController)

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
      waveFormController.addMarker("ad", waveFormController.cursorPosition)
    }
    contents += Button("Setup mock clock") {
      waveFormController.setClock(ClockInfo("mock clock", 10, 1))
    }
    contents += Button("Toggle Clock") {
      waveFormController.toggleClock()
    }
    contents += Button("Remove signal(s)") {
      inspectionContainer.removeSignals(this)
    }
    contents += Button("Add group") {
      waveFormController.addGroup()
    }
  }

  title = "Chisel Visualizer"
  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Open") {
        //TODO: IMPLEMENT THIS
      })
      contents += new MenuItem(Action("Restart") {
        //TODO: IMPLEMENT THIS
      })
      contents += new MenuItem(Action("Quit") {
        System.exit(0)
      })
    }
    contents += new Menu("Help") {
      contents += new Menu("Not so much yet")
    }

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

    val secondSplitPane: SplitPane = new SplitPane(Orientation.Vertical,
      splitPane,
      inputControlPanel
    ) {
      border = BorderFactory.createEmptyBorder()
    }

    layout(secondSplitPane) = Center
    layout(dependencyComponent) = South
    // layout(inputControlPanel) = East

    listenTo(waveFormController)
    listenTo(selectionController)
    reactions += {
      case DependencyComponentRequested(symbols, _) =>
        dependencyComponent.textComponent.text = symbols.map { symbol =>
          TreadleController.tester.get.dependencyInfo(symbol.name)
        }.mkString("\n")

      case SourceInfoRequested(symbols, _) =>
        dependencyComponent.textComponent.text = symbols.map { symbol =>
             s"Signal ${symbol.name} -- ${symbol.info}"
          }.mkString("\n")

      case e: MaxTimestampChanged =>
        inspectionContainer.zoomToEnd(this)
    }
  }
}
