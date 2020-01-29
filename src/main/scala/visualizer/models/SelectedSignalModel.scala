package visualizer.models

import scalaswingcontrib.tree.Tree.Path
import scalaswingcontrib.tree._
import treadle.executable.ClockInfo
import visualizer._
import visualizer.config.DrawMetrics

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing._

/** This model describes the selected and organized nodes that
  * have been selected to have their waveforms displayed
  *
  */
class SelectedSignalModel extends Publisher {
  // Maps nodeId to WaveDisplaySetting
  val waveDisplaySettings: mutable.HashMap[String, WaveDisplaySetting] =
    new mutable.HashMap[String, WaveDisplaySetting]()

  val RootPath:  Tree.Path[GenericTreeNode] = Tree.Path.empty[GenericTreeNode]
  val treeModel: InternalTreeModel[GenericTreeNode] = InternalTreeModel.empty[GenericTreeNode]

  var scale:             Double = 2
  var minorTickInterval: Long = 1

  var clkMinorTickInterval: Long = 1
  var useClock:             Boolean = false
  var clock:                Option[ClockInfo] = None

  var timeSieveOpt:                 Option[TimeSieve] = None
  var currentDecoupledSieveSignal:  String = ""
  var currentDecoupledSieveTrigger: BigInt = 0
  // initial/constructor
  setScale(10, null)

  ///////////////////////////////////////////////////////////////////////////
  // Helper Functions
  ///////////////////////////////////////////////////////////////////////////
  def xCoordinateToTimestamp(x: Int): Long = (x / scale).toLong

  def xCoordinateToSievedTimestamp(x: Int): Long = {
    val unsievedTimestamp = xCoordinateToTimestamp(x)
    timeSieveOpt match {
      case Some(timeSieve) =>
        timeSieve.logicalTimeToSieveTime(unsievedTimestamp)
      case _ =>
        unsievedTimestamp
    }
  }

  def timestampToXCoordinate(timestamp: Long): Int = (timestamp * scale).toInt

  def sievedTimestampToXCoordinate(sievedTimestamp: Long): Int = {
    val timestamp = timeSieveOpt match {
      case Some(timeSieve) =>
        timeSieve.sieveTimeToLogicalTime(sievedTimestamp)
      case _ =>
        sievedTimestamp
    }
    timestampToXCoordinate(timestamp)
  }

  /** This is a trap point for all nodes added to the wave form view.
    * This creates a Wave for each node, including any sub-nodes needed by complex nodes
    * E.g. Decoupled needs waves for ready and valid even though they are not directly visible
    *
    * @param parentPath  where to put it
    * @param nodeToAdd   what to put
    * @param index       index within the parent children
    */
  def insertUnder(parentPath: Path[GenericTreeNode], nodeToAdd: GenericTreeNode, index: Int): Unit = {
    treeModel.insertUnder(parentPath, nodeToAdd, index)
    nodeToAdd match {
      case WaveFormNode(_, signal, _) => signal.makeWaves()
      case _                          =>
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Signals
  ///////////////////////////////////////////////////////////////////////////

  /** This is how nodes are added from the signal selector
    *
    * @param addDirection       where to add based on current head of selection
    * @param genericTreeNode    node to add
    * @param source             swing component that is selector panel
    * @param targetPathOpt      overrides direction
    * @return
    */
  def addNodes(addDirection:    AddDirection,
               genericTreeNode: GenericTreeNode,
               source:          Component,
               targetPathOpt:   Option[Path[GenericTreeNode]] = None): Path[GenericTreeNode] = {

    val tree = ChiselGUI.mainWindow.signalAndWavePanel.tree

    def targetPath =
      if (targetPathOpt.isDefined) {
        targetPathOpt
      } else {
        addDirection match {
          case InsertBefore => tree.selection.paths.headOption
          case InsertInto   => tree.selection.paths.headOption
          case InsertAfter  => tree.selection.paths.lastOption
          case _            => Some(RootPath)
        }
      }

    /** Complicated code to figure out where to insert
      *
      * @return
      */
    def putTheNodeIn(genericTreeNode: GenericTreeNode, addDirection: AddDirection): Path[GenericTreeNode] = {
      val pathToNewNode = addDirection match {
        case InsertBefore =>
          targetPath match {
            case Some(path) =>
              treeModel.insertBefore(path, genericTreeNode)
              path.tail ++ Seq(genericTreeNode)
            case _ =>
              insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
              RootPath ++ Seq(genericTreeNode)
          }

        case InsertInto =>
          targetPath match {
            case Some(path) =>
              insertUnder(path, genericTreeNode, treeModel.getChildrenOf(path).size)
              path ++ Seq(genericTreeNode)
            case _ =>
              insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
              RootPath ++ Seq(genericTreeNode)
          }

        case InsertAfter =>
          targetPath match {
            case Some(path) =>
              treeModel.insertAfter(path, genericTreeNode)
              path.tail ++ Seq(genericTreeNode)
            case _ =>
              insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
              RootPath ++ Seq(genericTreeNode)
          }

        case AppendToEnd =>
          insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
          RootPath ++ Seq(genericTreeNode)

      }

      genericTreeNode match {
        case waveFormNode: WaveFormNode =>
          waveFormNode.signal match {
            case p: PureSignal =>
              waveDisplaySettings.getOrElseUpdate(p.name, WaveDisplaySetting())
              Waves.updateWave(waveFormNode.signal.name)
            case d: DecoupledSignalGroup =>
              Waves.updateWave(d.readySignal.name)
              Waves.updateWave(d.validSignal.name)
              d.bitsSignals.foreach { bitSignal =>
                Waves.updateWave(bitSignal.name)
              }
              d.updateValues()
            case v: ValidSignalGroup =>
              Waves.updateWave(v.validSignal.name)
              v.bitsSignals.foreach { bitSignal =>
                Waves.updateWave(bitSignal.name)
              }
              v.updateValues()
            case _ =>
          }
        case _ =>
      }

      pathToNewNode
    }

    val lastNodeAdded = putTheNodeIn(genericTreeNode, addDirection)

    publish(SignalsChanged(source))
    lastNodeAdded
  }

  def addGroup(newGroupName: String): Unit = {
    val node = DirectoryNode(newGroupName)
    insertUnder(RootPath, node, treeModel.getChildrenOf(RootPath).size)
  }

  // Removes all selected signals, selected groups, and children of selected groups
  // TODO: change type of paths?
  def removeSelectedSignals(source: Component, paths: Iterator[Tree.Path[GenericTreeNode]]): Unit = {
    paths.foreach { path =>
      treeModel.remove(path)
    }
    publish(SignalsChanged(source))
  }

  def moveSignals(source: Component): Unit = {
    publish(SignalsChanged(source))
  }

  ///////////////////////////////////////////////////////////////////////////
  // Create a hard-code time sieve based on fire events in Decoupled Group
  ///////////////////////////////////////////////////////////////////////////

  def createDecoupledTimeSieve(groupName: String, triggerValue: BigInt, enable: Boolean = true): Unit = {
    val timeSieve = new TimeSieve
    timeSieveOpt = if (enable) { Some(timeSieve) } else { None }
    Waves.get(groupName).foreach { wave =>
      var accumulatedTime = -1L
      wave.indices.foreach {
        case index if wave.value(index) == triggerValue =>
          val (start, end) = (wave.start(index), wave.end(index))
          if (accumulatedTime < 0) accumulatedTime = start
          timeSieve.add(start, end)
          accumulatedTime += start
        case _ => None
      }
      currentDecoupledSieveSignal = groupName
      currentDecoupledSieveTrigger = triggerValue
      ChiselGUI.mainWindow.toolbar.toggleSieve.visible = true
    }
  }

  def clearTimeSieve(): Unit = {
    timeSieveOpt = None
  }

  def usingSieve: Boolean = timeSieveOpt.isDefined

  ///////////////////////////////////////////////////////////////////////////
  // Wave Display Format
  ///////////////////////////////////////////////////////////////////////////
  def setWaveFormat(source: Component, nodes: Iterator[GenericTreeNode], format: Format): Unit = {
    nodes.foreach {
      case node: WaveFormNode =>
        node.signal match {
          case signal: PureSignal =>
            waveDisplaySettings(signal.name).dataFormat = Some(format)
          case _ =>
        }
      case _ =>
    }
    publish(WaveFormatChanged(source))
  }

  ///////////////////////////////////////////////////////////////////////////
  // Scale
  ///////////////////////////////////////////////////////////////////////////
  def setScale(newScale: Double, source: Component): Unit = {
    scale = newScale
    val x = math.pow(10, math.ceil(math.log10(DrawMetrics.MinMinorTickHSpace / scale))).toLong
    minorTickInterval = if (x <= 0) 1 else x
    publish(ScaleChanged(source))
  }

  ///////////////////////////////////////////////////////////////////////////
  // Timeline
  ///////////////////////////////////////////////////////////////////////////
  def setClock(newClock: ClockInfo): Unit = {
    // Verify wave matches a clock? (binary, oscillating)
    clock = Some(newClock)
    useClock = true
    publish(TimeUnitsChanged(null))
  }

  def removeClock(): Unit = {
    clock match {
      case Some(_) =>
        clock = None
        useClock = false
        publish(TimeUnitsChanged(null))
      case _ =>
    }
  }

  def toggleClock(): Unit = {
    clock match {
      case Some(_) =>
        useClock = !useClock
        publish(TimeUnitsChanged(null))
      case _ => // No clock set up
    }
  }

  def refreshTimeline(): Unit = {
    publish(TimeUnitsChanged(null))
  }

  var restrictedTransitionTimes: mutable.ArrayBuffer[Transition] = new mutable.ArrayBuffer()

  /** Currently only works on [[DecoupledSignalGroup]] will only show time on
    * wave view where fired is true for the given symbol
    *
    * @param genericTreeNode the managing nodes
    */
  def restrictTime(genericTreeNode: GenericTreeNode): Unit = {
    //    genericTreeNode match {
    //      case decoupledSignalGroup: DecoupledSignalGroup =>
    //        decoupledSignalGroup.waveform.foreach { case waveform: Waveform[Transition[BigInt]] =>
    //          var startTime = 0
    //        }
    //      case _ =>
    //        restrictedTransitionTimes.clear()
    //    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Cursor
  ///////////////////////////////////////////////////////////////////////////
  var cursorPosition: Long = 0

  /** if a sieve is in active use it map the timestamp
    *
    * @param timestamp  cursor time to save
    */
  def setCursorPosition(timestamp: Long): Unit = {
    cursorPosition = timeSieveOpt match {
      case Some(timeSieve) =>
        timeSieve.logicalTimeToSieveTime(timestamp)
      case _ =>
        timestamp
    }
    publish(CursorSet(null))
  }

  def getCursorPosition: Long = {
    cursorPosition
  }

  var selectionStart: Long = 0

  ///////////////////////////////////////////////////////////////////////////
  // Markers
  ///////////////////////////////////////////////////////////////////////////
  var markers: ArrayBuffer[Marker] = ArrayBuffer[Marker]()

  def removeAllMarkers(): Unit = {
    markers.clear
    publish(MarkerChanged(-1, null))
  }

  def addMarker(description: String, timestamp: Long): Unit = {
    // Adding to markers could be more efficient bc inserting into sorted sequence
    markers += Marker(description, timestamp)
    markers.sortBy(_.timestamp)

    publish(MarkerChanged(timestamp, null))
  }

  def removeMarker(description: String): Unit = {
    // Adding to markers could be more efficient bc inserting into sorted sequence
    markers = markers.filterNot { marker =>
      marker.description == description
    }
    publish(MarkerChanged(0, null))
  }

  // Returns index of element that matches
  def getMarkerAtTime(timestamp: Long): Int = {
    markers.reverse.indexWhere(timestamp >= _.timestamp) match {
      case i if i >= 0 => markers.size - 1 - i
      case _           => 0
    }
  }

  // Returns -1 if no marker was found near this timestamp.
  // Otherwise returns the index of the marker
  def findMarkerNear(timestamp: Long): Int = {
    val MarkerRemoveSlack: Long = (5.0 / scale).toLong

    if (markers.isEmpty) {
      -1
    } else {
      val index = getMarkerAtTime(timestamp)
      var targetTimeStamp = markers(index).timestamp
      if (math.abs(timestamp - targetTimeStamp) <= MarkerRemoveSlack) {
        index
      } else if (index < markers.size - 1) {
        targetTimeStamp = markers(index + 1).timestamp
        if (math.abs(timestamp - targetTimeStamp) <= MarkerRemoveSlack) {
          index + 1
        } else {
          -1
        }
      } else {
        -1
      }
    }
  }

  def removeMarkerAtTime(timestamp: Long): Unit = {
    val index = findMarkerNear(timestamp)
    if (index != -1) {
      val actualTimestamp = markers(index).timestamp
      markers.remove(index)
      publish(MarkerChanged(actualTimestamp, null))
    }
  }

  def prevMarker(extendSelection: Boolean): Unit = {
    val index = getMarkerAtTime(cursorPosition)
    var timestamp = markers(index).timestamp
    if (timestamp >= cursorPosition && index > 0) {
      timestamp = markers(index - 1).timestamp
    }

    setCursorPosition(timestamp)
    if (!extendSelection)
      selectionStart = timestamp
  }

  def nextMarker(extendSelection: Boolean): Unit = {
    val index = getMarkerAtTime(cursorPosition)
    if (index < markers.size - 1) {
      var timestamp = markers(index).timestamp
      if (timestamp <= cursorPosition) {
        timestamp = markers(index + 1).timestamp
      }

      setCursorPosition(timestamp)
      if (!extendSelection)
        selectionStart = timestamp
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Dependency Graph
  ///////////////////////////////////////////////////////////////////////////
  def showDependency(pureSignalName: String, source: Component): Unit = {
    publish(DependencyComponentRequested(pureSignalName, source))
  }
}

case class Marker(id: Int, var description: String, timestamp: Long)

object Marker {
  var nextMarkerId = 1

  def apply(name: String, timestamp: Long): Marker = {
    nextMarkerId += 1
    Marker(nextMarkerId, name, timestamp)
  }
}

case class WaveDisplaySetting(var painter: Option[Int] = None, var dataFormat: Option[Format] = None)

// May want to consider size of the wire
sealed trait Format {
  def apply(num: BigInt): String

  def radix: Int

  def radixChar: String
}

object Format {
  def serialize(format: Format): String = {
    format.toString
  }

  def serialize(formatOpt: Option[Format]): String = {
    if (formatOpt.isDefined) formatOpt.get.toString else "none"
  }

  def deserialize(string: String): Format = {
    string match {
      case "BinForma"   => BinFormat
      case "HexFormat"  => HexFormat
      case "DecFormat"  => DecFormat
      case "PlotFormat" => PlotFormat
      case _            => DecFormat
    }
  }
}

case object BinFormat extends Format {
  def apply(num: BigInt): String = {
    "0b" + num.toString(2)
  }

  def radix: Int = 2

  def radixChar: String = "b"
}

case object DecFormat extends Format {
  def apply(num: BigInt): String = {
    num.toString(10)
  }

  def radix: Int = 10

  def radixChar: String = "b"
}

case object HexFormat extends Format {
  def apply(num: BigInt): String = {
    "0x" + num.toString(16)
  }

  def radix: Int = 16

  def radixChar: String = "b"
}

case object PlotFormat extends Format {
  def apply(num: BigInt): String = {
    num.toString(10)
  }

  def radix: Int = 10

  def radixChar: String = "d"
}
