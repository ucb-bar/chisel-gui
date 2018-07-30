package visualizer

import java.io.File

import scalaswingcontrib.tree.Tree
import treadle.{TreadleOptionsManager, TreadleTester}
import treadle.repl.HasReplConfig
import visualizer.components.MainWindow
import visualizer.models._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, Publisher, SwingApplication}

object TreadleController extends SwingApplication with Publisher {
  var tester: Option[TreadleTester] = None
  private val clkSteps = 2

  val dataModel = new DataModel
  val displayModel = new DisplayModel
  lazy val mainWindow = new MainWindow(dataModel, displayModel)

  override def startup(args: Array[String]): Unit = {
    val t = mainWindow
    if (t.size == new Dimension(0, 0)) t.pack()
    t.visible = true

    // TODO: determine if args is info from treadle or vcd
    loadFirrtl()
    runSomeTreadle()
    setupWaveforms()
  }

  def loadFile(fileName: String): String = {
    var file = new File(fileName)
    if (!file.exists()) {
      file = new File(fileName + ".fir")
      if (! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    io.Source.fromFile(file).mkString
  }

  def loadFirrtl(fileName: String = "../../treadle/samples/gcd.fir"): Unit = {
    val optionsManager = new TreadleOptionsManager with HasReplConfig {
      treadleOptions = treadleOptions.copy(rollbackBuffers = clkSteps * 900)
    }
    val firrtlString = loadFile(fileName)
    tester = Some(treadle.TreadleTester(firrtlString, optionsManager))
    setupClock()
  }

  def setupClock(): Unit = {
    tester match {
      case Some(t) =>
        if (t.clockInfoList.nonEmpty) {
          displayModel.setClock(t.clockInfoList.head)
        }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Hard coded things
  ///////////////////////////////////////////////////////////////////////////
  def runSomeTreadle(): Unit = {
    tester match {
      case Some(t) =>
        for (a <- 10 to 20; b <- 20 to 22) {
          t.poke("io_e", 1)
          t.poke("io_a", a)
          t.poke("io_b", b)
          t.step()
          t.poke("io_e", 0)
          t.step(clkSteps)
        }
      case None =>
    }
  }

  def makeBinaryTransitions(times: ArrayBuffer[Int]): Waveform[BigInt] = {
    times.zipWithIndex.map{ case (time, index) =>
      Transition[BigInt](time, index % 2)
    }
  }

  val pureSignalMapping = new mutable.HashMap[String, DirectoryNode]
  def setupWaveforms(): Unit = {
    tester match {
      case Some(t) =>
        val wv = t.allWaveformValues
        Util.toValueChange(wv, initializing = true).foreach { case (name, waveform) =>
          val node = DirectoryNode(name, Some(new PureSignal(name, waveform)))
          pureSignalMapping(name) = node
          dataModel.directoryTreeModel.insertUnder(dataModel.RootPath, node, 0)
        }
        mainWindow.repaint()
        dataModel.updateMaxTimestamp()

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

        publish(new PureSignalsChanged)
    }
  }
}
