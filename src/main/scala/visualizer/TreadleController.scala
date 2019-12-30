package visualizer

import java.io.File

import firrtl.FileUtils
import firrtl.stage.FirrtlSourceAnnotation
import treadle.{RollBackBuffersAnnotation, TreadleOptionsManager, TreadleTester, WriteVcdAnnotation}
import treadle.repl.HasReplConfig
import visualizer.components.MainWindow
import visualizer.models._

import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, Publisher, SwingApplication}

object TreadleController extends SwingApplication with Publisher {
  var tester: Option[TreadleTester] = None
  private val clkSteps = 2

  val dataModel = new DataModel
  val displayModel = new DisplayModel
  lazy val mainWindow = new MainWindow(dataModel, displayModel)

  override def startup(args: Array[String]): Unit = {
    if (mainWindow.size == new Dimension(0, 0)) mainWindow.pack()
    mainWindow.visible = true

    // TODO: determine if args is info from treadle or vcd
    if (args.isEmpty) {
      hackySetup()
    } else {
      assert(args.length == 1)
      val firrtlString = FileUtils.getText(args.head)
      setupTreadle(firrtlString)
    }
  }

  def loadFile(fileName: String): String = {
    var file = new File(fileName)
    if (!file.exists()) {
      file = new File(fileName + ".fir")
      if (!file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    FileUtils.getText(file)
  }

  def addSignal(fullName: String, signal: Signal[_ <: Any]): Unit = {
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
    val node = DirectoryNode(signalName, Some(signal))
    dataModel.insertUnderSorted(parentPath, node)

    signal match {
      case pureSignal: PureSignal => dataModel.pureSignalMapping(fullName) = pureSignal
      case _ =>
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Treadle specific methods
  ///////////////////////////////////////////////////////////////////////////
  def setupTreadle(firrtlString: String): Unit = {
    val treadleTester = loadFirrtl(firrtlString)
    setupClock(treadleTester)
    setupSignals(treadleTester)
    setupWaveforms(treadleTester)
  }

  def loadFirrtl(firrtlString: String): TreadleTester = {
    val optionsManager = new TreadleOptionsManager with HasReplConfig {
      treadleOptions = treadleOptions.copy(rollbackBuffers = clkSteps * 900)
    }
    val t = treadle.TreadleTester(
      Seq(
        FirrtlSourceAnnotation(firrtlString),
        WriteVcdAnnotation
      )
    )
    tester = Some(t)
    t
  }

  def setupClock(t: TreadleTester): Unit = {
    if (t.clockInfoList.nonEmpty) {
      displayModel.setClock(t.clockInfoList.head)
    }
  }

  def setupSignals(tester: TreadleTester): Unit = {
    tester.engine.symbolTable.nameToSymbol.foreach { case (name, symbol) =>
      if (! name.contains("/")) {
        val sortGroup = Util.sortGroup(name, tester)
        val arrayBuffer = new ArrayBuffer[Transition[BigInt]]()
        arrayBuffer += Transition(0L, BigInt(0))
        val signal = new PureSignal(name, Some(symbol), Some(new Waveform(arrayBuffer)), sortGroup)
        addSignal(name, signal)
      }
    }
  }

  def setupWaveforms(t: TreadleTester): Unit = {
    t.engine.vcdOption match {
      case Some(vcd) =>
        Util.vcdToTransitions(vcd, initializing = true).foreach {
          case (fullName, transitions) =>
            if (dataModel.pureSignalMapping.contains(fullName)) {
              dataModel.pureSignalMapping(fullName).addNewValues(transitions)
            }
        }
      case _ =>
    }

    mainWindow.repaint()
    dataModel.updateMaxTimestamp()
    publish(new PureSignalsChanged)
  }

  ///////////////////////////////////////////////////////////////////////////
  // Hard coded things
  ///////////////////////////////////////////////////////////////////////////
  def runSomeTreadle(t: TreadleTester): Unit = {
    for {
      a <- 10 to 20
      b <- 20 to 22
    } {
      t.poke("io_e", 1)
      t.poke("io_a", a)
      t.poke("io_b", b)
      t.step()
      t.poke("io_e", 0)
      t.step(clkSteps)
    }
  }

  def makeBinaryTransitions(times: ArrayBuffer[Int]): Waveform[BigInt] = {
    val transitions = times.zipWithIndex.map {
      case (time, index) =>
        Transition[BigInt](time, index % 2)
    }
    new Waveform(transitions)
  }

  def hackySetup(): Unit = {
    val firrtlString = loadFile("samples/gcd.fir")
    val treadleTester = loadFirrtl(firrtlString)
    setupClock(treadleTester)
    runSomeTreadle(treadleTester)
    setupWaveforms(treadleTester)

    val waveformReady = makeBinaryTransitions(ArrayBuffer[Int](0, 16, 66, 106, 136, 176, 306, 386, 406, 496, 506))
    val signalReady = new PureSignal("ready", None, Some(waveformReady), 0)
    addSignal("module.io_fake_ready", signalReady)

    val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
    val signalValid = new PureSignal("valid", None, Some(waveformValid), 0)
    addSignal("module.io_fake_valid", signalValid)

    val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
    addSignal("module.io_rv", signalRV)

    publish(new PureSignalsChanged)
  }
}
