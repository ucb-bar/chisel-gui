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

  val pureSignalMapping = new mutable.HashMap[String, DirectoryNode]
  def addSignal(fullName: String, pureSignal: Signal[_ <: Any]): Unit = {
    // the full name of the signal (from treadle) uses periods to separate modules
    val fullPath = fullName.split("\\.")
    val signalName = fullPath.last
    val modules = fullPath.init

    val parentPath = modules.foldLeft(dataModel.RootPath) { (parentPath, module) =>
      val node = DirectoryNode(module, None)
      val children = dataModel.directoryTreeModel.getChildrenOf(parentPath)
      if (!children.contains(node)) {
        dataModel.insertUnderSorted(parentPath, node)
      }
      parentPath :+ node
    }
    val node = DirectoryNode(signalName, Some(pureSignal))
    dataModel.insertUnderSorted(parentPath, node)

    pureSignal match {
      case _: PureSignal => pureSignalMapping(fullName) = node
      case _ =>
    }
  }

  def setupWaveforms(): Unit = {
    tester match {
      case Some(t) =>
        val wv = t.allWaveformValues
        Util.toValueChange(wv, initializing = true).foreach { case (fullName, transitions) =>
          val waveform = if (transitions.nonEmpty) Some(new Waveform(transitions)) else None
          val signal = new PureSignal(fullName, waveform, t.isRegister(fullName))
          addSignal(fullName, signal)
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

  def hackySetupWaveforms(): Unit = {
    setupWaveforms()

    val waveformReady = makeBinaryTransitions(ArrayBuffer[Int](0, 16, 66, 106, 136, 176, 306, 386, 406, 496, 506))
    val signalReady = new PureSignal("ready", Some(waveformReady), false)
    addSignal("module.io_fake_ready", signalReady)

    val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
    val signalValid = new PureSignal("valid", Some(waveformValid), false)
    addSignal("module.io_fake_valid", signalValid)

    val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
    addSignal("module.io_rv", signalRV)

    publish(new PureSignalsChanged)
  }
}
