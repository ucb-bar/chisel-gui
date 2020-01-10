package visualizer

import java.io.File

import firrtl.ir.ClockType
import firrtl.options.{ProgramArgsAnnotation, Shell}
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.{AnnotationSeq, FileUtils, InstanceKind, MemKind}
import treadle.executable.{Symbol, SymbolTable}
import treadle.vcd.VCD
import treadle.{TreadleTester, WriteVcdAnnotation}
import visualizer.components.MainWindow
import visualizer.models._
import visualizer.stage.{ChiselGuiCli, ChiselSourcePaths, VcdFile}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, Publisher, SwingApplication}

object TreadleController extends SwingApplication with Publisher {
  val shell: Shell = new Shell("chisel-gui") with ChiselGuiCli

  var testerOpt: Option[TreadleTester] = None
  var vcdOpt: Option[treadle.vcd.VCD] = None

  val dataModel: DataModel = new DataModel
  val signalSelectorModel: SignalSelectorModel = new SignalSelectorModel
  val selectedSignalModel: SelectedSignalModel = new SelectedSignalModel

  val sourceInfoMap: mutable.HashMap[String, String] = new mutable.HashMap()

  lazy val mainWindow = new MainWindow(dataModel, signalSelectorModel, selectedSignalModel)

  override def startup(args: Array[String]): Unit = {
    val startAnnotations = shell.parse(args)

    if (mainWindow.size == new Dimension(0, 0)) mainWindow.pack()
    mainWindow.visible = true

    val firrtlFileNameOpt = startAnnotations.collectFirst { case a: ProgramArgsAnnotation => a.arg }

    val vcdFileNameOpt = startAnnotations.collectFirst { case a: VcdFile => a.vcdFileName }

    startAnnotations.collectFirst { case pathAnnotation: ChiselSourcePaths => pathAnnotation.paths }.foreach { paths =>
      paths.foreach { path =>
        SourceFinder.walk(path).foreach { scalaPath =>
          sourceInfoMap(scalaPath.getName) = scalaPath.getAbsolutePath
        }
      }
    }

    (firrtlFileNameOpt, vcdFileNameOpt) match {
      case (Some(firrtlFileName), Some(vcdFile)) =>
        setupTreadle(firrtlFileName, startAnnotations)
        setupVcdInput(vcdFile)
      case (Some(firrtlFileName), None) =>
        setupTreadle(firrtlFileName, startAnnotations)
      case (None, Some(vcdFile)) =>
        setupVcdInput(vcdFile)
      case _ =>
      // Cannot get here. Should be trapped by shell.parse

    }
    signalSelectorModel.updateTreeModel()
    populateWaveForms()
    signalSelectorModel.updateTreeModel()
    loadSaveFileOnStartUp()
    mainWindow.signalSelectorPanel.updateModel()
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

  /**
    * looks through the save file and populates the selected signal container
    */
  def loadSaveFileOnStartUp(): Unit = {
    // This list is root -> leaf
    var currentPath = selectedSignalModel.RootPath
    var lastNode: GenericTreeNode = new GenericTreeNode {
      override def name: String = ""
    }
    var currentDepth = 1

    def addNode(depthString: String, indexString: String, node: GenericTreeNode): Unit = {
      val depth = depthString.toInt
      var index = indexString.toInt

      if (depth > currentDepth) {
        currentPath = currentPath ++ Seq(lastNode)
        currentDepth = depth
      } else if (depth < currentDepth) {
        currentPath = currentPath.take(depth - 1)
        currentDepth = depth
      }

      try {
        selectedSignalModel.treeModel.insertUnder(currentPath, node, index)
      }
      catch {
        case t: Throwable =>
          println(t.getMessage)
          println(s"depthString $depthString indexString $indexString $node")
      }
      lastNode = node
    }

    testerOpt.foreach { tester =>
      val fileNameGuess = new File(tester.topName + ".save")
      if (fileNameGuess.exists()) {
        FileUtils.getLines(fileNameGuess).foreach { line =>
          val fields = line.split(",").map(_.trim).toList
          fields match {
            case "window_size" :: widthString :: heightString :: Nil =>
              val size = new Dimension(widthString.toInt, heightString.toInt)
              mainWindow.size = size
              mainWindow.preferredSize = size

            case "signal_node" :: depthString :: indexString :: nodeName :: signalName :: formatString :: Nil =>
              dataModel.nameToSignal.get(signalName) match {
                case Some(pureSignal: PureSignal) =>
                  val node = WaveFormNode(nodeName, pureSignal)
                  selectedSignalModel.waveDisplaySettings(signalName) = {
                    WaveDisplaySetting(None, Some(Format.deserialize(formatString)))
                  }
                  addNode(depthString, indexString, node)
                case Some(_: CombinedSignal) =>
                case _ =>
              }

            case "node" :: depthString :: indexString :: nodeName :: Nil =>
              val node = DirectoryNode(nodeName)
              addNode(depthString, indexString, node)

            case "marker" :: timeString :: Nil =>
              try {
                selectedSignalModel.addMarker("ad", timeString.toLong)
              } catch {
                case _: Throwable =>
              }
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

  /** Spin-up a treadle instance and load the dataModel from the treadle
    * symbolTable.
    *
    * @param firrtlFileName Name of a file containing firrtl text
    * @param annotations    Annotation that have been provided from command line
    */
  def setupTreadle(firrtlFileName: String, annotations: AnnotationSeq): Unit = {
    val firrtlString = FileUtils.getText(firrtlFileName)
    val treadleTester = treadle.TreadleTester(
      Seq(
        FirrtlSourceAnnotation(firrtlString),
        WriteVcdAnnotation
      ) ++
        annotations
    )
    testerOpt = Some(treadleTester)
    setupSignalsFromTreadle()
    setupClock(treadleTester)
  }

  /** Get any initializing VCD data,
    * If there is a treadle tester then seed it's values from this vcd
    * possibly load vcd signals into dataModel
    * otherwise setup dataModels signals from the vcd signal names
    *
    * @param vcdFileName name of a file containing VCD text
    */
  def setupVcdInput(vcdFileName: String): Unit = {
    val vcd = treadle.vcd.VCD.read(vcdFileName)

    testerOpt match {
      case Some(tester) =>
        seedFromVcd(vcd, stopAtTime = Long.MaxValue)
        dataModel.setMaxTimestamp(vcd.valuesAtTime.keys.max)
        vcdOpt = tester.engine.vcdOption
      case _ =>
        setupSignalsFromVcd(vcd)
        vcdOpt = Some(vcd)
    }
  }

  def setupClock(t: TreadleTester): Unit = {
    if (t.clockInfoList.nonEmpty) {
      selectedSignalModel.setClock(t.clockInfoList.head)
    }
  }

  def setupSignalsFromTreadle(): Unit = {
    testerOpt.foreach { tester =>
      tester.engine.symbolTable.nameToSymbol.foreach {
        case (name, symbol) if symbol.dataKind != InstanceKind && symbol.dataKind != MemKind =>
          if (!name.contains("/")) {
            val sortGroup = Util.sortGroup(name, testerOpt)
            val arrayBuffer = new ArrayBuffer[Transition[BigInt]]()
            arrayBuffer += Transition(0L, BigInt(0))
            val signal = new PureSignal(name, Some(symbol), Some(new Waveform(arrayBuffer)), sortGroup)

            dataModel.addSignal(name, signal)
          }
        case _ =>
      }
    }

    println("signals loaded from tester")
  }

  def setupSignalsFromVcd(vcd: VCD): Unit = {
    vcd.wires.values.foreach { wire =>
      val name = wire.fullName
      val sortGroup = Util.sortGroup(name, testerOpt)
      val arrayBuffer = new ArrayBuffer[Transition[BigInt]]()
      arrayBuffer += Transition(0L, BigInt(0))
      val signal = new PureSignal(name, None, Some(new Waveform(arrayBuffer)), sortGroup)

      dataModel.addSignal(name, signal)
    }
    println("signals loaded")
  }

  def populateWaveForms(): Unit = {
    vcdOpt match {
      case Some(vcd) =>
        Util.vcdToTransitions(vcd, initializing = true).foreach {
          case (fullName, transitions) =>
            dataModel.nameToSignal.get(fullName) match {
              case Some(pureSignal: PureSignal) =>
                pureSignal.addNewValues(transitions)
              case Some(_: CombinedSignal) =>
              //TODO: figure out if anything needs to happen here
              case _ =>
            }
        }
        val newMaxTimestamp = if (vcd.valuesAtTime.nonEmpty) vcd.valuesAtTime.keys.max else 0L
        dataModel.setMaxTimestamp(newMaxTimestamp)
      case _ =>
    }

    publish(new PureSignalsChanged)
    mainWindow.repaint()
  }

  def loadDrivingSignals(signal: PureSignal): Unit = {
    val maxDepth = 3
    testerOpt.foreach { tester =>
      val engine = tester.engine
      val digraph = engine.symbolTable.parentsOf

      val table = engine.symbolTable
      val symbol = engine.symbolTable(signal.name)
      val symbolsSeen = new mutable.HashSet[String]()
      val symbolsAtDepth = Array.fill(maxDepth + 1) {
        new mutable.HashSet[Symbol]
      }

      symbolsSeen += signal.name
      walkGraph(symbol, depth = 0)

      def walkGraph(symbol: Symbol, depth: Int): Unit = {
        symbolsAtDepth(depth) += symbol

        if (depth < maxDepth) {
          digraph.getEdges(symbol).toSeq.sortBy(_.name).foreach { childSymbol =>
            walkGraph(childSymbol, depth + 1)

            if (table.isRegister(symbol.name)) {
              walkGraph(table(SymbolTable.makeRegisterInputName(symbol)), depth + 1)
            }
          }
        }

        val showDepth = symbolsAtDepth.count(_.nonEmpty)
        for (depth <- 0 until showDepth) {
          var added = 0

          println(s"driving symbols at distance $depth")
          symbolsAtDepth(depth).toSeq.map(_.name).filterNot(symbolsSeen.contains).sorted.foreach { signalName =>
            dataModel.nameToSignal.get(signalName).foreach { drivingSignal =>
              print(signalName)
              added += 1
              symbolsSeen += signalName
              val otherNode = WaveFormNode(signalName, drivingSignal)
              selectedSignalModel.addFromDirectoryToInspected(otherNode, mainWindow.signalSelectorPanel)
            }
          }
          if (added > 0) println()
        }
      }
    }
  }

  //  ///////////////////////////////////////////////////////////////////////////
  //  // Hard coded things
  //  ///////////////////////////////////////////////////////////////////////////
  //  def runSomeTreadle(t: TreadleTester): Unit = {
  //    for {
  //      a <- 10 to 20
  //      b <- 20 to 22
  //    } {
  //      t.poke("io_e", 1)
  //      t.poke("io_a", a)
  //      t.poke("io_b", b)
  //      t.step()
  //      t.poke("io_e", 0)
  //      t.step(clkSteps)
  //    }
  //  }
  //
  //  def makeBinaryTransitions(times: ArrayBuffer[Int]): Waveform[BigInt] = {
  //    val transitions = times.zipWithIndex.map {
  //      case (time, index) =>
  //        Transition[BigInt](time, index % 2)
  //    }
  //    new Waveform(transitions)
  //  }
  //
  ////  def hackySetup(): Unit = {
  ////    val firrtlString = loadFile("samples/gcd.fir")
  ////    val treadleTester = loadFirrtl(firrtlString)
  ////    setupClock(treadleTester)
  ////    runSomeTreadle(treadleTester)
  ////    populateWaveForms(treadleTester)
  ////
  ////    val waveformReady = makeBinaryTransitions(ArrayBuffer[Int](0, 16, 66, 106, 136, 176, 306, 386, 406, 496, 506))
  ////    val signalReady = new PureSignal("ready", None, Some(waveformReady), 0)
  ////    val fakeReady = "module.io_fake_ready"
  ////    dataModel.addSignal(fakeReady, signalReady)
  ////    selectionModel.addSignalToSelectionList(fakeReady, signalReady)
  ////
  ////    val waveformValid = makeBinaryTransitions(ArrayBuffer[Int](0, 36, 66, 96, 116, 146, 206, 286, 396, 406, 506))
  ////    val signalValid = new PureSignal("valid", None, Some(waveformValid), 0)
  ////    dataModel.addSignal("module.io_fake_valid", signalValid)
  ////
  ////    val signalRV = ReadyValidCombiner(Array[PureSignal](signalReady, signalValid))
  ////    dataModel.addSignal("module.io_rv", signalRV)
  ////
  ////    publish(new PureSignalsChanged)
  ////  }
}
