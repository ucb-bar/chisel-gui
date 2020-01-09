package visualizer.models

import scalaswingcontrib.tree.Tree.Path

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scalaswingcontrib.tree._
import treadle.executable.ClockInfo
import visualizer._

import scala.collection.mutable

/** This model describes the selected and organized nodes that
  * have been selected to have their waveforms displayed
  *
  */
class SelectedSignalModel extends Publisher {
  // Maps nodeId to WaveDisplaySetting
  val waveDisplaySettings: mutable.HashMap[String, WaveDisplaySetting] =
    new mutable.HashMap[String, WaveDisplaySetting]()

  val RootPath: Tree.Path[GenericTreeNode] = Tree.Path.empty[GenericTreeNode]
  val treeModel: InternalTreeModel[GenericTreeNode] = InternalTreeModel.empty[GenericTreeNode]

  var scale: Double = 2
  var minorTickInterval: Long = 1

  var clkMinorTickInterval: Long = 1
  var useClock: Boolean = false
  var clock: Option[ClockInfo] = None

  // initial/constructor
  setScale(10, null)

  ///////////////////////////////////////////////////////////////////////////
  // Helper Functions
  ///////////////////////////////////////////////////////////////////////////
  def xCoordinateToTimestamp(x: Int): Long = (x / scale).toLong

  def timestampToXCoordinate(timestamp: Long): Int = (timestamp * scale).toInt

  ///////////////////////////////////////////////////////////////////////////
  // Signals
  ///////////////////////////////////////////////////////////////////////////
  def addFromDirectoryToInspected(node: GenericTreeNode, source: Component): Unit = {
    val inspectedNode = node
    treeModel.insertUnder(RootPath, inspectedNode, treeModel.getChildrenOf(RootPath).size)

    node match {
      case waveFormNode: WaveFormNode =>
        waveDisplaySettings.getOrElseUpdate(waveFormNode.name, WaveDisplaySetting())
      case _ =>
    }
    publish(SignalsChanged(source)) // TODO: Rename to NodesChanged
  }

  def addNodes(addDirection: AddDirection, genericTreeNode: GenericTreeNode, source: Component, targetPathOpt: Option[Path[GenericTreeNode]] = None): Path[GenericTreeNode] = {

    val tree = TreadleController.mainWindow.signalAndWavePanel.tree

    def targetPath = if (targetPathOpt.isDefined) {
      targetPathOpt
    } else {
      addDirection match {
        case InsertBefore => tree.selection.paths.headOption
        case InsertInto => tree.selection.paths.headOption
        case InsertAfter => tree.selection.paths.lastOption
        case _ => Some(RootPath)
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
              treeModel.insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
              RootPath ++ Seq(genericTreeNode)
          }

        case InsertInto =>
          targetPath match {
            case Some(path) =>
              treeModel.insertUnder(path, genericTreeNode, treeModel.getChildrenOf(path).size)
              path ++ Seq(genericTreeNode)
            case _ =>
              treeModel.insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
              RootPath ++ Seq(genericTreeNode)
          }

        case InsertAfter =>
          targetPath match {
            case Some(path) =>
              treeModel.insertAfter(path, genericTreeNode)
              path.tail ++ Seq(genericTreeNode)
            case _ =>
              treeModel.insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
              RootPath ++ Seq(genericTreeNode)
          }

        case AppendToEnd =>
          treeModel.insertUnder(RootPath, genericTreeNode, treeModel.getChildrenOf(RootPath).size)
          RootPath ++ Seq(genericTreeNode)

      }

      genericTreeNode match {
        case waveFormNode: WaveFormNode =>
          waveFormNode.signal match {
            case p: PureSignal =>
              waveDisplaySettings.getOrElseUpdate(p.name, WaveDisplaySetting())
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

  def addGroup(): Unit = {
    val node = DirectoryNode("New Group")
    treeModel.insertUnder(RootPath, node, treeModel.getChildrenOf(RootPath).size)
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

  ///////////////////////////////////////////////////////////////////////////
  // Cursor
  ///////////////////////////////////////////////////////////////////////////
  var cursorPosition: Long = 0
  def setCursorPosition(timestamp: Long): Unit = {
    cursorPosition = timestamp
    publish(CursorSet(null))
  }

  var selectionStart: Long = 0

  ///////////////////////////////////////////////////////////////////////////
  // Markers
  ///////////////////////////////////////////////////////////////////////////
  var markers: ArrayBuffer[Marker] = ArrayBuffer[Marker]()
  var nextMarkerId = 1

  def removeAllMarkers(): Unit = {
    markers.clear
    publish(MarkerChanged(-1, null))
    nextMarkerId = 1
  }

  def addMarker(description: String, timestamp: Long): Unit = {
    // Adding to markers could be more efficient bc inserting into sorted sequence
    markers += Marker(nextMarkerId, description, timestamp)
    markers.sortBy(_.timestamp)

    nextMarkerId += 1
    publish(MarkerChanged(timestamp, null))
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

case class WaveDisplaySetting(var painter: Option[Int] = None, var dataFormat: Option[Format] = None)

// May want to consider size of the wire
sealed trait Format {
  def apply(num: BigInt): String
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
      case "bin" => BinFormat
      case "hex" => HexFormat
      case "dec" => DecFormat
      case _ => DecFormat
    }
  }
}

case object BinFormat extends Format {
  def apply(num: BigInt): String = {
    "0b" + num.toString(2)
  }
}

case object DecFormat extends Format {
  def apply(num: BigInt): String = {
    num.toString(10)
  }
}
case object HexFormat extends Format {
  def apply(num: BigInt): String = {
    "0x" + num.toString(16)
  }
}
