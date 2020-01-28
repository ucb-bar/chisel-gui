package visualizer

import java.awt.TrayIcon
import java.io.File

import com.apple.eawt.Application
import firrtl.ir.ClockType
import firrtl.options.{ProgramArgsAnnotation, Shell}
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.{AnnotationSeq, FileUtils, InstanceKind, MemKind}
import javax.imageio.ImageIO
import javax.swing.UIManager
import scalaswingcontrib.tree.Tree
import treadle.executable.{Symbol, SymbolTable, VcdHook}
import treadle.vcd.VCD
import treadle.{TreadleTester, WriteVcdAnnotation}
import visualizer.components.MainWindow
import visualizer.config.ColorTable
import visualizer.models._
import visualizer.stage.{ChiselGuiCli, ChiselSourceOpenCommand, ChiselSourcePaths, VcdFile}

import scala.collection.mutable
import scala.swing.Dialog.Result
import scala.swing.{Dialog, Dimension, Publisher, SwingApplication}

object ChiselGUI extends SwingApplication with Publisher {
  try {
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "ChiselGUI")
    System.setProperty("apple.laf.useScreenMenuBar", "true")
    System.setProperty("apple.eawt.quitStrategy", "CLOSE_ALL_WINDOWS")
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  } catch {
    case t: Throwable =>
      // Failed to setup OS-X keep going
      println(s"Startup complaint:${t.getMessage}")
  }
  val shell: Shell = new Shell("chisel-gui") with ChiselGuiCli

  val saveFilePrefix = "."
  val saveFileSuffix = ".chiselgui"

  var testerOpt:       Option[TreadleTester] = None
  var vcdOpt:          Option[treadle.vcd.VCD] = None
  var rollupDecoupled: Boolean = true

  val dataModel:           DataModel = new DataModel
  val signalSelectorModel: SignalSelectorModel = new SignalSelectorModel
  val selectedSignalModel: SelectedSignalModel = new SelectedSignalModel

  val sourceInfoMap:     mutable.HashMap[String, String] = new mutable.HashMap()
  var sourceOpenCommand: Seq[String] = Seq.empty

  var mainWindow: MainWindow = _
  var mainWindowSize = new Dimension(1000, 600)
  var startupMarkers = new mutable.ArrayBuffer[Marker]()

  var startupCursorPosition:         Long = 0L
  var startupScale:                  Double = 10.0
  var startupVisibleX:               Int = -1
  var startUpColorScheme:            String = "default"
  var startupAggregateDecoupledFlag: Boolean = true
  var startupShowSignalSelector:     Boolean = true
  var startupSieveSignal:            String = ""
  var startupSieveTrigger:           BigInt = BigInt(0)
  val startupPokeHistory:            mutable.ArrayBuffer[mutable.HashMap[String, String]] = new mutable.ArrayBuffer()

  var toExpand = new mutable.ArrayBuffer[Tree.Path[GenericTreeNode]]()

  var startupWarnings: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer()

  override def startup(args: Array[String]): Unit = {
    val startAnnotations = shell.parse(args)

    val firrtlFileNameOpt = startAnnotations.collectFirst { case a: ProgramArgsAnnotation => a.arg }

    val vcdFileNameOpt = startAnnotations.collectFirst { case a: VcdFile => a.vcdFileName }

    sourceOpenCommand = startAnnotations.collectFirst { case ChiselSourceOpenCommand(paths) => paths }.getOrElse(
      Seq(
        "/Applications/IntelliJ IDEA.app/Contents/MacOS/idea",
        "--line",
        s"[[LINE]]",
        s"[[FILE]]"
      )
    )

    if (firrtlFileNameOpt.isEmpty && vcdFileNameOpt.isEmpty) {}

    startAnnotations.collectFirst { case pathAnnotation: ChiselSourcePaths => pathAnnotation.paths }.foreach { paths =>
      paths.foreach { path =>
        SourceFinder.walk(path).foreach { scalaPath =>
          sourceInfoMap(scalaPath.getName) = scalaPath.getAbsolutePath
        }
      }
    }

    var headerBarTitle: String = "ChiselGUI"

    //
    // firrtl and vcd resources are read in here and dataModel is populated
    //
    (firrtlFileNameOpt, vcdFileNameOpt) match {
      case (Some(firrtlFileName), Some(vcdFile)) =>
        setupTreadle(firrtlFileName, startAnnotations)
        setupVcdInput(vcdFile)
        headerBarTitle = s"$firrtlFileName - $vcdFile"
      case (Some(firrtlFileName), None) =>
        setupTreadle(firrtlFileName, startAnnotations)
        vcdOpt = testerOpt.get.engine.vcdOption
        Waves.setVcd(vcdOpt.get)
        headerBarTitle = s"$firrtlFileName"
      case (None, Some(vcdFile)) =>
        setupVcdInput(vcdFile)
        headerBarTitle = s"$vcdFile"
      case _ =>
    }

    addDecoupledSignals()

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
    ChiselGUI.signalSelectorModel.setRollupDecoupled(startupAggregateDecoupledFlag)

    mainWindow.title = headerBarTitle

    signalSelectorModel.dataModelFilter = signalSelectorModel.dataModelFilter.copy(
      hiddenDecoupled = DecoupledHandler.signalNameToDecouple.values.flatMap(_.getChildNames).toSeq
    )

    signalSelectorModel.updateTreeModel()
    mainWindow.signalSelectorPanel.updateModel()

    populateWaveForms()

    if (startupScale > 0.0 && startupVisibleX >= 0) {
      mainWindow.signalAndWavePanel.setScaleAndVisible(startupScale, startupVisibleX)
    }

    if (startUpColorScheme != "default") {
      ColorTable.setAltWaveColors()
    }

    expandNodesOnStartup()

    if (startupSieveSignal.nonEmpty) {
      selectedSignalModel.createDecoupledTimeSieve(groupName = startupSieveSignal, startupSieveTrigger)
    }

    startupPokeHistory.foreach { pokes =>
      dataModel.savePokeValues(pokes.toMap)
    }
    startupPokeHistory.lastOption.foreach { pokes =>
      mainWindow.inputControlPanel.setTextBoxes(pokes.toMap)
    }
    mainWindow.inputControlPanel.setHistoryLabel()

    publish(new PureSignalsChanged)

    startupWarnings.foreach { warning =>
      val result = Dialog.showConfirmation(mainWindow, s"$warning\nContinue?", "Warning during startup")
      if (result != Result.Yes) {
        System.exit(1)
      }
    }

    mainWindow.signalSelectorPanel.tree.requestFocus()
    mainWindow.signalSelectorPanel.tree.requestFocusInWindow()

    setDockIcon()
  }

  def addDecoupledSignals(): Unit = {
    DecoupledHandler.signalNameToDecouple.foreach {
      case (name, decoupledHandler) =>
        try {
          (decoupledHandler.readyNameOpt, decoupledHandler.validNameOpt) match {
            case (Some(readyName), Some(validName)) =>
              val decoupledSignal = new DecoupledSignalGroup(
                s"${decoupledHandler.prefix}/RV",
                dataModel.nameToSignal(readyName).asInstanceOf[PureSignal],
                dataModel.nameToSignal(validName).asInstanceOf[PureSignal],
                decoupledHandler.bits.map { bitsName =>
                  dataModel.nameToSignal(bitsName).asInstanceOf[PureSignal]
                }
              )
              dataModel.addSignal(decoupledSignal.name, decoupledSignal)
            case (None, Some(validName)) =>
              val validSignal = new ValidSignalGroup(
                s"${decoupledHandler.prefix}/V",
                dataModel.nameToSignal(validName).asInstanceOf[PureSignal],
                decoupledHandler.bits.map { bitsName =>
                  dataModel.nameToSignal(bitsName).asInstanceOf[PureSignal]
                }
              )
              dataModel.addSignal(validSignal.name, validSignal)
          }

        } catch {
          case t: Throwable =>
            println(s"Unable to add $decoupledHandler")
        }
    }
  }

  def seedFromVcd(vcd: treadle.vcd.VCD, stopAtTime: Long = Long.MaxValue): Unit = {
    val engine = testerOpt.get.engine
    val wallTime = testerOpt.get.wallTime

    var lastClockTransitionTime = 0L
    var clockHalfPeriodGuess = 0L
    var lastClockValue = 0L

    engine.dataStore.plugins.values.foreach {
      case vcdHook: VcdHook => vcdHook.setEnabled(false)
      case _ =>
    }

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

    engine.dataStore.plugins.values.foreach {
      case vcdHook: VcdHook => vcdHook.setEnabled(true)
      case _ =>
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
        selectedSignalModel.insertUnder(currentPath, node, index)
        node match {
          case w: WaveFormNode => Waves.addEntryFor(w.name)
          case _ =>
        }
      } catch {
        case t: Throwable =>
          println(t.getMessage)
          println(s"depthString $depthString indexString $indexString $node")
        //          throw t
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

            case "signal_node" :: depthString :: indexString ::
                  nodeName :: signalName :: formatString :: expand :: Nil =>
              dataModel.nameToSignal.get(signalName) match {
                case Some(pureSignal: PureSignal) =>
                  val node = WaveFormNode(nodeName, pureSignal)
                  selectedSignalModel.waveDisplaySettings(signalName) = {
                    WaveDisplaySetting(None, Some(Format.deserialize(formatString)))
                  }
                  addNode(depthString, indexString, node)
                  if (expand == "expand") {
                    toExpand += currentPath ++ Seq(node)
                  }
                case _ =>
              }

            case "decoupled_node" :: depthString :: indexString ::
                  nodeName :: signalName :: formatString :: expand :: Nil =>
              dataModel.nameToSignal.get(signalName) match {
                case Some(decoupledSignalGroup: DecoupledSignalGroup) =>
                  val node = WaveFormNode(nodeName, decoupledSignalGroup)
                  selectedSignalModel.waveDisplaySettings(signalName) = {
                    WaveDisplaySetting(None, Some(Format.deserialize(formatString)))
                  }
                  addNode(depthString, indexString, node)
                  if (expand == "expand") {
                    toExpand += currentPath ++ Seq(node)
                  }
                case _ =>
              }

            case "valid_node" :: depthString :: indexString ::
                  nodeName :: signalName :: formatString :: expand :: Nil =>
              dataModel.nameToSignal.get(signalName) match {
                case Some(validSignalGroup: ValidSignalGroup) =>
                  val node = WaveFormNode(nodeName, validSignalGroup)
                  selectedSignalModel.waveDisplaySettings(signalName) = {
                    WaveDisplaySetting(None, Some(Format.deserialize(formatString)))
                  }
                  addNode(depthString, indexString, node)
                  if (expand == "expand") {
                    toExpand += currentPath ++ Seq(node)
                  }
                case _ =>
              }

            case "node" :: depthString :: indexString :: nodeName :: expand :: Nil =>
              val node = DirectoryNode(nodeName)
              addNode(depthString, indexString, node)
              if (expand == "expand") {
                toExpand += currentPath ++ Seq(node)
              }

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
                case _: Throwable =>
                  println(s"Cannot parse line $line from ${fileNameGuess.getName}")
              }

            case "aggregate_decoupled" :: boolString :: Nil =>
              startupAggregateDecoupledFlag = boolString.toBoolean

            case "show_signal_selector" :: boolString :: Nil =>
              startupShowSignalSelector = boolString.toBoolean

            case "decoupled_sieve_signal" :: name :: triggerString :: Nil =>
              startupSieveSignal = name
              startupSieveTrigger = BigInt(triggerString)

            case "poke_history" :: tail =>
              val pokeMap = new mutable.HashMap[String, String]()
              tail.sliding(2, 2).foreach {
                case key :: value :: Nil =>
                  pokeMap += key -> value
                case _ =>
              }
              if (pokeMap.nonEmpty) {
                startupPokeHistory += pokeMap
              }
            case _ =>
              println(s"Invalid line $line in save file")

          }
        }
      }
    }
  }

  def expandNodesOnStartup(): Unit = {
    toExpand.foreach { path =>
      mainWindow.signalAndWavePanel.tree.expandPath(path)
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
    if (new File(firrtlFileName).exists) {
      val firrtlString = FileUtils.getText(firrtlFileName)
      val treadleTester = treadle.TreadleTester(
        Seq(
          FirrtlSourceAnnotation(firrtlString),
          WriteVcdAnnotation
        ) ++
          annotations
      )
      testerOpt = Some(treadleTester)

      DecoupledHandler.lookForReadyValidBundles(treadleTester.engine.symbolTable.nameToSymbol.keys.toSeq)
      setupSignalsFromTreadle()
      setupClock(treadleTester)
    } else {
      val warning = s"Could not open FIRRTL file $firrtlFileName"
      println(warning)
      startupWarnings += warning

    }
  }

  /** Get any initializing VCD data,
    * If there is a treadle tester then seed it's values from this vcd
    * possibly load vcd signals into dataModel
    * otherwise setup dataModels signals from the vcd signal names
    *
    * @param vcdFileName name of a file containing VCD text
    */
  def setupVcdInput(vcdFileName: String): Unit = {
    if (new File(vcdFileName).exists) {
      val vcd = treadle.vcd.VCD.read(vcdFileName)

      testerOpt match {
        case Some(tester) =>
          seedFromVcd(vcd, stopAtTime = Long.MaxValue)
          dataModel.setMaxTimestamp(vcd.valuesAtTime.keys.max)
          vcdOpt = tester.engine.vcdOption
          Waves.setVcd(tester.engine.vcdOption.get)
        case _ =>
          DecoupledHandler.lookForReadyValidBundles(vcd.wires.keys.toSeq)
          setupSignalsFromVcd(vcd)
          vcdOpt = Some(vcd)
          Waves.setVcd(vcd)
      }
    } else {
      val warning = s"Could not open VCD file $vcdFileName"
      println(warning)
      startupWarnings += warning
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
            val signal = PureSignal(name, Some(symbol), symbol.bitWidth)
            dataModel.addSignal(name, signal)
          }
        case _ =>
      }
    }
  }

  def setupSignalsFromVcd(vcd: VCD): Unit = {
    vcd.wires.values.foreach { wire =>
      val name = wire.fullName
      val signal = PureSignal(name, None, wire.width)

      dataModel.addSignal(name, signal)
    }
  }

  def populateWaveForms(): Unit = {
    dataModel.loadMoreWaveformValues()
  }

  def loadDrivingSignals(signal: PureSignal, maxDepth: Int): Seq[GenericTreeNode] = {
    testerOpt match {
      case Some(tester) =>
        val engine = tester.engine
        val digraph = engine.symbolTable.parentsOf

        val table = engine.symbolTable
        val symbol = engine.symbolTable(signal.name)
        val symbolsSeen = new mutable.HashSet[String]()
        val symbolsAtDepth = Array.fill(maxDepth + 1) {
          new mutable.HashSet[Symbol]
        }

        val nodesToAdd = new mutable.ArrayBuffer[GenericTreeNode]()

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
            symbolsAtDepth(depth).toSeq.map(_.name).filterNot(symbolsSeen.contains).sorted.foreach { signalName =>
              dataModel.nameToSignal.get(signalName).foreach { drivingSignal =>
                symbolsSeen += signalName
                val otherNode = WaveFormNode(signalName, drivingSignal)
                nodesToAdd += otherNode
              }
            }
          }
        }
        nodesToAdd
    case _ =>
      Seq.empty
  }
  }

  /** This changes the dock and the command-tab and has been tested on osx 🚩
    */
  //TODO: Test this on other systems
  def setDockIcon(): Unit = {
    try {
      val application = Application.getApplication
      val r = ImageIO.read(getClass.getResource("/images/chisel-gui-icon.png"))
      application.setDockIconImage(r)
    } catch {
      case t: Throwable =>
        println(t.getMessage)
    }
  }

  /** This adds an icon to the top menu bar tray on osx, no real reason to use it at this time
    */
  def addTrayIcon(): Unit = {
    import java.awt.SystemTray

    if (!SystemTray.isSupported) {
      println("SystemTray is not supported")
    } else {
      println("SystemTray IS supported")
      val r = ImageIO.read(getClass.getResource("/images/chisel-gui-icon.png"))
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
