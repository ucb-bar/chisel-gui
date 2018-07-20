package visualizer

import java.awt.Color

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

  val dataModel = new DataModel
  val displayModel = new DisplayModel
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
      displayModel.setClock(-1)
    }
    contents += Button("Toggle Clock") {
      displayModel.toggleClock()
    }
    contents += Button("Remove signal(s)") {
      inspectionContainer.removeSignals(this)
    }
  }

  lazy val ui: BorderPanel = new BorderPanel {
    import BorderPanel.Position._

    background = Color.white
    preferredSize = (800, 600)

    focusable = true

    layout(toolbar) = North

    val splitPane = new SplitPane(Orientation.Vertical,
      new ScrollPane(directoryContainer), inspectionContainer)

    layout(splitPane) = Center
  }

  def top: MainFrame = new MainFrame {
    title = "Chisel Visualizer"

    menuBar = new MenuBar {
      contents += new Menu("File")
    }

    contents = ui

    hacky()

  }

  def hacky(): Unit = {
    runSomeTreadle match {
      case Some(wv: WaveformValues) =>
        Util.toValueChange(wv).foreach { case (name, waveform) =>
          val node = DirectoryNode(name, Some(new PureSignal(name, waveform)))
          dataModel.directoryTreeModel.insertUnder(dataModel.RootPath, node, 0)
        }
        dataModel.updateMaxTimestamp()
        directoryContainer.repaint()

        // testing submodules
        val module = DirectoryNode("module", None)
        dataModel.directoryTreeModel.insertUnder(dataModel.RootPath, module, 0)

        val waveformReady = makeBinaryTransitions(ArrayBuffer[Int](0, 16, 66, 106, 136, 176, 306, 386, 406, 496, 506))
        val signalReady = new PureSignal("ready", waveformReady)
        val nodeReady = DirectoryNode("io_fake_ready", Some(signalReady))
        dataModel.directoryTreeModel.insertUnder(Tree.Path(module), nodeReady, 0)

        val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
        val signalValid = new PureSignal("valid", waveformValid)
        val nodeValid = DirectoryNode("io_fake_valid", Some(signalValid))
        dataModel.directoryTreeModel.insertUnder(Tree.Path(module), nodeValid, 0)

        val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
        val nodeRV = DirectoryNode("io_rv", Some(signalRV))
        dataModel.directoryTreeModel.insertUnder(Tree.Path(module), nodeRV, 0)
      case _ =>
    }
  }

  def makeBinaryTransitions(times: ArrayBuffer[Int]): Waveform[BigInt] = {
    times.zipWithIndex.map{ case (time, index) =>
      Transition[BigInt](time, index % 2)
    }
  }

  def runSomeTreadle: Option[WaveformValues] = {
    val w = 100
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

    treadleAPI.executeCommand(Array("waves", "0", w.toString, "clk", "io_a", "io_b", "io_e", "x", "y", "io_z", "io_v"))
  }
}
