package visualizer

import java.awt.Color

import javax.swing.tree.DefaultMutableTreeNode
import scalaswingcontrib.tree._
import visualizer.components._
import visualizer.models._

import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing._
import scala.swing._
import treadle._
import treadle.executable.WaveformValues
import treadle.repl.HasReplConfig

object Main extends SimpleSwingApplication {

  val dataModel = new InspectionDataModel
  val displayModel = new InspectionDisplayModel
  val directoryContainer = new DirectoryComponent(dataModel, displayModel)
  val inspectionContainer = new InspectionContainer(dataModel, displayModel)

  val toolbar = new ToolBar() {
    contents += Button("Zoom In") {
      displayModel.zoomIn(this)
    }
    contents += Button("Zoom Out") {
      displayModel.zoomOut(this)
    }
    contents += Button("Add Marker") {
      displayModel.addMarker("ad", displayModel.cursorPosition)
    }
  }

  lazy val ui = new BorderPanel {
    import BorderPanel.Position._

    background = Color.white
    preferredSize = (800, 600)

    focusable = true

    layout(toolbar) = North

    val splitPane = new SplitPane(Orientation.Vertical,
      new ScrollPane(directoryContainer), inspectionContainer)

    layout(splitPane) = Center

//    layout(directoryContainer) = West
//    layout(inspectionContainer) = Center

  }

  def top = new MainFrame {
    title = "Chisel Visualizer"

    menuBar = new MenuBar {
      contents += new Menu("File")
    }

    contents = ui

    hacky

  }

  def hacky: Unit = {
    runSomeTreadle match {
      case Some(wv: WaveformValues) => {

        Util.toValueChange(wv).values.zipWithIndex.foreach { case (waveform, index) =>
          dataModel.waveforms(index) = waveform
          dataModel.directoryTreeModel.insertUnder(dataModel.RootPath, DirectoryNode(index, waveform.name), index)
        }
        dataModel.updateMaxTimestamp
        directoryContainer.update


        // testing submodules
        val module = DirectoryNode(-1, "module")
        dataModel.directoryTreeModel.insertUnder(dataModel.RootPath, module, 0)
        dataModel.waveforms(100) = Waveform("fake", ArrayBuffer[Transition](Transition(0, 0), Transition(106, 99)))
        dataModel.directoryTreeModel.insertUnder(Tree.Path(module), DirectoryNode(100, "io_fake"), 0)

        // very hacky. The directory is not painted if there are no initial nodes. So we add a temporary node and remove it
        // need to move inside InspectionDataModel
        dataModel.directoryTreeModel.remove(Tree.Path(dataModel.temporaryNode))

      }
      case _ =>
    }
  }

  def runSomeTreadle: Option[WaveformValues] = {
    val w = 40
    val clkSteps = 5

    val optionsManager = new TreadleOptionsManager with HasReplConfig {
      treadleOptions = treadleOptions.copy(rollbackBuffers = clkSteps * 900)
    }

    val treadleAPI = new TreadleAPI(optionsManager)
    treadleAPI.executeCommand(Array("load", "../../treadle/samples/gcd.fir"))

    for (a <- 10 to 20; b <- 20 to 30) {
      treadleAPI.executeCommand(Array("poke", "io_e", "1"))
      treadleAPI.executeCommand(Array("poke", "io_a", a.toString))
      treadleAPI.executeCommand(Array("poke", "io_b", b.toString))
      treadleAPI.executeCommand(Array("step"))
      treadleAPI.executeCommand(Array("poke", "io_e", "0"))
      treadleAPI.executeCommand(Array("step", clkSteps.toString))
    }

    treadleAPI.executeCommand(Array("waves", "0", w.toString, "io_a", "io_b", "io_e", "x", "y", "io_z", "io_v"))
  }
}
