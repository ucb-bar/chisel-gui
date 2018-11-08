package visualizer

import java.io.File

import treadle.{TreadleOptionsManager, TreadleTester}
import treadle.repl.HasReplConfig
import visualizer.components.MainWindow
import visualizer.controllers.{SelectionController, WaveFormController}
import visualizer.models._

import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, Publisher, SwingApplication}

object TreadleController extends SwingApplication with Publisher {
  var tester: Option[TreadleTester] = None
  private val clkSteps = 2

  val selectionController = new SelectionController
  val waveFormController = new WaveFormController
  lazy val mainWindow = new MainWindow(selectionController, waveFormController)

  override def startup(args: Array[String]): Unit = {
    if (mainWindow.size == new Dimension(0, 0)) mainWindow.pack()
    mainWindow.visible = true

    // TODO: determine if args is info from treadle or vcd
    if (args.isEmpty) {
      hackySetup()
    } else {
      assert(args.length == 1)
      val firrtlString = io.Source.fromFile(args.head).getLines().mkString("\n")
      setupTreadle(firrtlString)
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

  def getPureSignal(node: SelectionNode): Option[PureSignal] = {
    None
  }

  def addSelection(node: SelectionNode): Unit = {
    val fullPath = node.name.split("""\.""")
    val modules = fullPath.init

    val parentPath = modules.foldLeft(selectionController.RootPath) { (parentPath, module) =>
      val node = SelectionGroup(module)
      val children = selectionController.directoryTreeModel.getChildrenOf(parentPath)
      if (!children.contains(node)) {
        selectionController.insertUnderSorted(parentPath, node)
      }
      parentPath :+ node
    }
    selectionController.insertUnderSorted(parentPath, node)
  }

  ///////////////////////////////////////////////////////////////////////////
  // Treadle specific methods
  ///////////////////////////////////////////////////////////////////////////
  def setupTreadle(firrtlString: String): Unit = {
    val treadleTester = loadFirrtl(firrtlString)
    setupClock(treadleTester)
    setupWaveforms(treadleTester)
  }

  def loadFirrtl(firrtlString: String): TreadleTester = {
    val optionsManager = new TreadleOptionsManager with HasReplConfig {
      treadleOptions = treadleOptions.copy(rollbackBuffers = clkSteps * 900)
    }
    val t = treadle.TreadleTester(firrtlString, optionsManager)
    tester = Some(t)
    t
  }

  def setupClock(t: TreadleTester): Unit = {
    if (t.clockInfoList.nonEmpty) {

      waveFormController.setClock(t.clockInfoList.head)
    }
  }

  def setupWaveforms(t: TreadleTester): Unit = {
//    val wv = t.allWaveformValues
//    Util.toValueChange(wv, initializing = true).foreach { case (fullName, transitions) =>
//      if (!fullName.contains("/")) {
//        val waveform = if (transitions.nonEmpty) Some(new Waveform(transitions)) else None
//        val sortGroup = if (t.isRegister(fullName)) {
//          1
//        } else {
//          val signalName = fullName.split("\\.").last
//          if (signalName.contains("io_")) {
//            0
//          } else if (signalName.contains("T_") || signalName.contains("GEN_")) {
//            3
//          } else {
//            2
//          }
//        }
//        val signal = new PureSignal(fullName, waveform, sortGroup)
//        addSignal(fullName, signal)
//      }
//    }

    t.engine.symbolTable.symbols.foreach { symbol =>
//      selectionController.addSymbol(symbol)
      addSelection(SelectionSignal(symbol))
    }
    mainWindow.repaint()
    waveFormController.updateMaxTimestamp()
    publish(new PureSignalsChanged)
  }

  ///////////////////////////////////////////////////////////////////////////
  // Hard coded things
  ///////////////////////////////////////////////////////////////////////////
  def runSomeTreadle(t: TreadleTester): Unit = {
    for (a <- 10 to 20; b <- 20 to 22) {
      t.poke("io_e", 1)
      t.poke("io_a", a)
      t.poke("io_b", b)
      t.step()
      t.poke("io_e", 0)
      t.step(clkSteps)
    }
  }

  def makeBinaryTransitions(times: ArrayBuffer[Int]): Waveform[BigInt] = {
    val transitions = times.zipWithIndex.map{ case (time, index) =>
      Transition[BigInt](time, index % 2)
    }
    new Waveform(transitions)
  }

  def hackySetup(): Unit = {
    val firrtlString = loadFile("../treadle/samples/gcd.fir")
    val treadleTester = loadFirrtl(firrtlString)
    setupClock(treadleTester)
    runSomeTreadle(treadleTester)
    setupWaveforms(treadleTester)

//    val waveformReady = makeBinaryTransitions(ArrayBuffer[Int](0, 16, 66, 106, 136, 176, 306, 386, 406, 496, 506))
//    val signalReady = new PureSignal("ready", Some(waveformReady), 0)
//    addSignal("module.io_fake_ready", signalReady)
//
//    val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
//    val signalValid = new PureSignal("valid", Some(waveformValid), 0)
//    addSignal("module.io_fake_valid", signalValid)
//
//    val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
//    addSignal("module.io_rv", signalRV)

    publish(new PureSignalsChanged)
  }
}
