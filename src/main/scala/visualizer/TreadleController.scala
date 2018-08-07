package visualizer

import java.io.File

//import com.bulenkov.darcula.DarculaLaf
//import javax.swing.UIManager
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
    //UIManager.setLookAndFeel(new DarculaLaf)

    val t = mainWindow
    if (t.size == new Dimension(0, 0)) t.pack()
    t.visible = true

    // TODO: determine if args is info from treadle or vcd
    if (args.isEmpty) {
      val firrtlString = loadFile("../../treadle/samples/gcd.fir")
      loadFirrtl(firrtlString)
      runSomeTreadle()
      setupWaveforms()
      hackySetupWaveforms()
    } else {
      assert(args.length == 1)
      val firrtlString = args(0)
      loadFirrtl(firrtlString)
      setupWaveforms()
    }
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

  def loadFirrtl(firrtlString: String): Unit = {
    val optionsManager = new TreadleOptionsManager with HasReplConfig {
      treadleOptions = treadleOptions.copy(rollbackBuffers = clkSteps * 900)
    }
    tester = Some(treadle.TreadleTester(firrtlString, optionsManager))
    setupClock()
  }

  def setupClock(): Unit = {
    tester match {
      case Some(t) =>
        if (t.clockInfoList.nonEmpty) {
          displayModel.setClock(t.clockInfoList.head)
        }
      case None =>
    }
  }

  def setupWaveforms(): Unit = {
    tester match {
      case Some(t) =>
        val wv = t.allWaveformValues
        Util.toValueChange(wv, initializing = true).foreach { case (name, transitions) =>
          val signal = if (transitions.nonEmpty) {
            new PureSignal(name, Some(new Waveform(transitions)), t.isRegister(name))
          } else {
            new PureSignal(name, None, t.isRegister(name))
          }
          val node = DirectoryNode(name, Some(signal))
          pureSignalMapping(name) = node
          dataModel.insertUnderSorted(dataModel.RootPath, node)
        }
        mainWindow.repaint()
        dataModel.updateMaxTimestamp()

        publish(new PureSignalsChanged)
      case None =>
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
    val transitions = times.zipWithIndex.map{ case (time, index) =>
      Transition[BigInt](time, index % 2)
    }
    new Waveform(transitions)
  }

  val pureSignalMapping = new mutable.HashMap[String, DirectoryNode]
  def hackySetupWaveforms(): Unit = {
    tester match {
      case Some(t) =>
        // testing submodules
        val module = DirectoryNode("module", None)
        dataModel.insertUnderSorted(dataModel.RootPath, module)

        val waveformReady = makeBinaryTransitions(ArrayBuffer[Int](0, 16, 66, 106, 136, 176, 306, 386, 406, 496, 506))
        val signalReady = new PureSignal("ready", Some(waveformReady), false)
        val nodeReady = DirectoryNode("io_fake_ready", Some(signalReady))
        dataModel.insertUnderSorted(Tree.Path(module), nodeReady)

        val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
        val signalValid = new PureSignal("valid", Some(waveformValid), false)
        val nodeValid = DirectoryNode("io_fake_valid", Some(signalValid))
        dataModel.insertUnderSorted(Tree.Path(module), nodeValid)

        val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
        val nodeRV = DirectoryNode("io_rv", Some(signalRV))
        dataModel.insertUnderSorted(Tree.Path(module), nodeRV)

        publish(new PureSignalsChanged)
      case None =>
    }
  }
}
