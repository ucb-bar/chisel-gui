package visualizer

import java.io.File

import firrtl.FileUtils
import firrtl.ir.ClockType
import firrtl.stage.FirrtlSourceAnnotation
import treadle.executable.SymbolTable
import treadle.{TreadleTester, WriteVcdAnnotation}
import visualizer.components.MainWindow
import visualizer.models._

import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, Publisher, SwingApplication}

object TreadleController extends SwingApplication with Publisher {
  var testerOpt: Option[TreadleTester] = None
  private val clkSteps = 2

  val dataModel = new DataModel
  val selectionModel = new SelectionModel
  val displayModel = new DisplayModel
  lazy val mainWindow = new MainWindow(dataModel, selectionModel, displayModel)

  override def startup(args: Array[String]): Unit = {
    if (mainWindow.size == new Dimension(0, 0)) mainWindow.pack()
    mainWindow.visible = true

    // TODO: determine if args is info from treadle or vcd
    args.toList match {

      case firrtlFileName :: vcdFileName :: Nil =>
        val vcd = treadle.vcd.VCD.read(vcdFileName)
        val firrtlString = FileUtils.getText(firrtlFileName)
        setupTreadle(firrtlString)
        testerOpt match {
          case Some(tester) =>
            seedFromVcd(vcd, stopAtTime = Long.MaxValue)
            dataModel.setMaxTimestamp(vcd.valuesAtTime.keys.max)
          case _ =>
        }
        setupWaveforms(testerOpt.get)
        selectionModel.updateTreeModel()
        loadSaveFileOnStartUp()
        mainWindow.signalSelector.updateModel()

      case firrtlFileName :: Nil =>
        val firrtlString = FileUtils.getText(firrtlFileName)
        setupTreadle(firrtlString)
        setupWaveforms(testerOpt.get)
        selectionModel.updateTreeModel()
        loadSaveFileOnStartUp()
        mainWindow.signalSelector.updateModel()

      case Nil =>
        hackySetup()
        setupWaveforms(testerOpt.get)
        selectionModel.updateTreeModel()
        mainWindow.signalSelector.updateModel()

      case _ =>
        println("Usage: chisel-gui firrtlFile [vcdFile]")
        System.exit(1)
    }
  }

  def seedFromVcd(vcd: treadle.vcd.VCD, stopAtTime: Long = Long.MaxValue): Unit = {
    val engine = testerOpt.get.engine
    val wallTime = testerOpt.get.wallTime

    var lastClockTransitionTime = 0L
    var clockHalfPeriodGuess = 0L
    var lastClockValue = 0L

    vcd.valuesAtTime.keys.toSeq.sorted.foreach { time =>
      for (change <- vcd.valuesAtTime(time)) {
        if (time <= stopAtTime) {
          wallTime.setTime(time)

          engine.symbolTable.get(change.wire.fullName) match {
            case Some(symbol) =>
              engine.setValue(symbol.name, change.value, force = true)
              if (symbol.firrtlType == ClockType) {
                clockHalfPeriodGuess = time - lastClockTransitionTime
                lastClockTransitionTime = time
                lastClockValue = change.value.toLong
                val prevName = SymbolTable.makePreviousValue(symbol)
                engine.setValue(prevName, change.value)
              }

            case _ =>
              println(s"Could not find symbol for $change")
          }
        }
      }
    }
    if (lastClockValue == 0L) {
      testerOpt.get.advanceTime(clockHalfPeriodGuess)
    }
  }

  def loadSaveFileOnStartUp(): Unit = {
    testerOpt.foreach { tester =>
      val fileNameGuess = new File(tester.topName + ".save")
      if (fileNameGuess.exists()) {
        FileUtils.getLines(fileNameGuess).foreach { line =>
          val fields = line.split(",").map(_.trim).toList
          fields match {
            case "node" :: signalName :: format :: Nil =>
              dataModel.pureSignalMapping.get(signalName).foreach { signal =>
                //                InspectedNode.nameToNode.get(signalName).foreach {
                //                  case node: InspectedNode =>
                //                    displayModel.addFromDirectoryToInspected(node, mainWindow.inspectionContainer)
                //                  case _ =>
                //                }
              }
            case "marker" :: time =>
            case _ =>
              println(s"Invalid line $line in save file")

          }
        }
      }
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

  ///////////////////////////////////////////////////////////////////////////
  // Treadle specific methods
  ///////////////////////////////////////////////////////////////////////////
  def setupTreadle(firrtlString: String): Unit = {
    val treadleTester = loadFirrtl(firrtlString)
    setupClock(treadleTester)
    setupSignals(treadleTester)
  }

  def loadFirrtl(firrtlString: String): TreadleTester = {
    val t = treadle.TreadleTester(
      Seq(
        FirrtlSourceAnnotation(firrtlString),
        WriteVcdAnnotation
      )
    )
    testerOpt = Some(t)
    t
  }

  def setupClock(t: TreadleTester): Unit = {
    if (t.clockInfoList.nonEmpty) {
      displayModel.setClock(t.clockInfoList.head)
    }
  }

  def setupSignals(tester: TreadleTester): Unit = {
    tester.engine.symbolTable.nameToSymbol.foreach {
      case (name, symbol) =>
        if (!name.contains("/")) {
          val sortGroup = Util.sortGroup(name, tester)
          val arrayBuffer = new ArrayBuffer[Transition[BigInt]]()
          arrayBuffer += Transition(0L, BigInt(0))
          val signal = new PureSignal(name, Some(symbol), Some(new Waveform(arrayBuffer)), sortGroup)

          dataModel.addSignal(name, signal)
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
        dataModel.setMaxTimestamp(vcd.valuesAtTime.keys.max)
      case _ =>
    }

    publish(new PureSignalsChanged)
    mainWindow.repaint()
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
    val fakeReady = "module.io_fake_ready"
    dataModel.addSignal(fakeReady, signalReady)
    selectionModel.addSignalToSelectionList(fakeReady, signalReady)

    val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
    val signalValid = new PureSignal("valid", None, Some(waveformValid), 0)
    dataModel.addSignal("module.io_fake_valid", signalValid)

    val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
    dataModel.addSignal("module.io_rv", signalRV)

    publish(new PureSignalsChanged)
  }
}
