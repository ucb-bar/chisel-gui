package visualizer

import java.awt.TrayIcon
import java.io.File

import firrtl.ir.ClockType
import firrtl.options.{ProgramArgsAnnotation, Shell}
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.{AnnotationSeq, FileUtils, InstanceKind, MemKind}
import javax.imageio.ImageIO
import treadle.executable.{Symbol, SymbolTable}
import treadle.vcd.VCD
import treadle.{TreadleTester, WriteVcdAnnotation}
import visualizer.components.MainWindow
import visualizer.config.ColorTable
import visualizer.models._
import visualizer.stage.{ChiselGuiCli, ChiselSourcePaths, VcdFile}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.{Dimension, Publisher, SwingApplication}

object ChiselGUI extends SwingApplication with Publisher {
  System.setProperty("apple.eawt.quitStrategy", "CLOSE_ALL_WINDOWS")

  setApplicationIcon()

  val shell: Shell = new Shell("chisel-gui") with ChiselGuiCli

  val saveFilePrefix = ".chiselgui."
  val saveFileSuffix = ".txt"

  var testerOpt: Option[TreadleTester] = None
  var vcdOpt: Option[treadle.vcd.VCD] = None

  val dataModel: DataModel = new DataModel
  val signalSelectorModel: SignalSelectorModel = new SignalSelectorModel
  val selectedSignalModel: SelectedSignalModel = new SelectedSignalModel

  val sourceInfoMap: mutable.HashMap[String, String] = new mutable.HashMap()

  var mainWindow: MainWindow = _
  var mainWindowSize = new Dimension(1000, 600)
  var startupMarkers = new mutable.ArrayBuffer[Marker]()
  var startupCursorPosition: Long = 0L
  var startupScale: Double = 10.0
  var startupVisibleX: Int = -1
  var startUpColorScheme: String = "default"

  override def startup(args: Array[String]): Unit = {
    val startAnnotations = shell.parse(args)

    val firrtlFileNameOpt = startAnnotations.collectFirst { case a: ProgramArgsAnnotation => a.arg }

    val vcdFileNameOpt = startAnnotations.collectFirst { case a: VcdFile => a.vcdFileName }

    if (firrtlFileNameOpt.isEmpty && vcdFileNameOpt.isEmpty) {}

    startAnnotations.collectFirst { case pathAnnotation: ChiselSourcePaths => pathAnnotation.paths }.foreach { paths =>
      paths.foreach { path =>
        SourceFinder.walk(path).foreach { scalaPath =>
          sourceInfoMap(scalaPath.getName) = scalaPath.getAbsolutePath
        }
      }
    }

    val headerBarTitle = (firrtlFileNameOpt, vcdFileNameOpt) match {
      case (Some(firrtlFileName), Some(vcdFile)) =>
        setupTreadle(firrtlFileName, startAnnotations)
        setupVcdInput(vcdFile)
        s"$firrtlFileName - $vcdFile"
      case (Some(firrtlFileName), None) =>
        setupTreadle(firrtlFileName, startAnnotations)
        s"$firrtlFileName"
      case (None, Some(vcdFile)) =>
        setupVcdInput(vcdFile)
        s"$vcdFile"
      case _ =>
        // Cannot get here. Should be trapped by shell.parse
        "ChiselGUI"
    }

    loadSaveFileOnStartUp()
    mainWindow = new MainWindow(dataModel, signalSelectorModel, selectedSignalModel)
    if (mainWindow.size == new Dimension(0, 0)) mainWindow.pack()
    mainWindow.size = mainWindowSize
    mainWindow.preferredSize = mainWindowSize
    mainWindow.visible = true
    selectedSignalModel.cursorPosition = startupCursorPosition
    startupMarkers.foreach { markerValue =>
      selectedSignalModel.markers += markerValue
    }

    mainWindow.title = headerBarTitle

    populateWaveForms()
    signalSelectorModel.updateTreeModel()
    mainWindow.signalSelectorPanel.updateModel()

    if (startupScale > 0.0 && startupVisibleX >= 0) {
      mainWindow.signalAndWavePanel.setScaleAndVisible(startupScale, startupVisibleX)
    }

    if (startUpColorScheme != "default") {
      ColorTable.setAltWaveColors()
    }

    publish(new PureSignalsChanged)

    mainWindow.signalSelectorPanel.tree.requestFocus()
    mainWindow.signalSelectorPanel.tree.requestFocusInWindow()

    setApplicationIcon()
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

      override def nodeId: Long = 0L
    }
    var currentDepth = 1

    def addNode(depthString: String, indexString: String, node: GenericTreeNode): Unit = {
      val depth = depthString.toInt
      val index = indexString.toInt

      if (depth > currentDepth) {
        currentPath = currentPath ++ Seq(lastNode)
        currentDepth = depth
      } else if (depth < currentDepth) {
        currentPath = currentPath.take(depth - 1)
        currentDepth = depth
      }

      try {
        selectedSignalModel.treeModel.insertUnder(currentPath, node, index)
      } catch {
        case t: Throwable =>
          println(t.getMessage)
          println(s"depthString $depthString indexString $indexString $node")
          throw t
      }
      lastNode = node
    }

    testerOpt.foreach { tester =>
      val fileNameGuess = new File(saveFilePrefix + tester.topName + saveFileSuffix)
      if (fileNameGuess.exists()) {
        FileUtils.getLines(fileNameGuess).foreach { line =>
          val fields = line.split(",").map(_.trim).toList
          fields match {
            case "window_size" :: widthString :: heightString :: Nil =>
              mainWindowSize = new Dimension(widthString.toInt, heightString.toInt)

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

            case "cursor-position" :: positionString :: Nil =>
              try {
                startupCursorPosition += positionString.toLong
              } catch {
                case _: Throwable =>
              }

            case "marker" :: markerName :: timeString :: Nil =>
              try {
                val time = timeString.toLong
                startupMarkers += Marker(markerName, time)
              } catch {
                case _: Throwable =>
              }

            case "wave-colors" :: colorScheme :: Nil =>
              startUpColorScheme = colorScheme

            case "scale-and-window" :: scaleString :: xString :: Nil =>
              try {
                startupScale = scaleString.toDouble
                startupVisibleX = xString.toInt
              } catch {
                case t: Throwable =>
                  println(s"Cannot parse line $line from ${fileNameGuess.getName}")
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
  }

  def loadDrivingSignals(signal: PureSignal, maxDepth: Int): Unit = {
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

  /** Try to change the system icon for the application
    * This is a noble effort but does not seem to work
    * no complaints from system but no change either
    */
  def setApplicationIcon(): Unit = {
    import java.awt.SystemTray

    if (!SystemTray.isSupported) {
      println("SystemTray is not supported")
    } else {
      println("SystemTray IS supported")
      val r = ImageIO.read(getClass.getResource("/images/bulb.gif"))
      val tray = SystemTray.getSystemTray

      val trayIcon = new TrayIcon(r)

      try {
        tray.add(trayIcon)
      } catch {
        case t: Throwable =>
          println(s"Error trying to set app icon: ${t.getMessage}")
      }
    }
  }
}