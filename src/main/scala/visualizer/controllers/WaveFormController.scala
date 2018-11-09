package visualizer.controllers

import javax.swing.event.{TreeExpansionEvent, TreeExpansionListener}
import javax.swing.tree.{DefaultMutableTreeNode, TreePath}
import javax.swing.{DropMode, SwingUtilities}

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scalaswingcontrib.tree._
import treadle.executable.ClockInfo
import visualizer._
import visualizer.components.{InspectionContainer, SignalNameRenderer}
import visualizer.models._

import scala.collection.mutable
import scala.swing.event.MouseClicked

/**
  * The controller for the wave form viewer
  * manages the list of signals selected for viewing and the wave forms themselves
  * responds to signal being added from the signal selector
  * and wave forms being changed by the input panel
  */
class WaveFormController extends Publisher {
  // Maps node to WaveDisplaySetting
  val waveDisplaySettings: mutable.HashMap[SelectionNode, WaveDisplaySetting]   = new mutable.HashMap()
  val waveFormDataMap:     mutable.HashMap[SelectionNode, Signal[_]] = new mutable.HashMap()

  val treeModel: SignalSelectionModel     = new SignalSelectionModel
  val RootPath:  Tree.Path[SelectionNode] = SelectionNode.RootPath

  val waveFormController: WaveFormController = this

  def getMaxTimeStamp: Long = {
    TreadleController.tester match {
      case Some(t) =>
        t.wallTime.currentTime
      case _ => 0L
    }
  }

  // Popup menu when a signal name is right-clicked
  private def popupMenu(signal: Option[Signal[_ <: Any]]): PopupMenu = new PopupMenu {
    contents += new Menu("Data Format") {
      contents += new MenuItem(Action("Binary") {
        waveFormController.setWaveFormat(this, tree.selection.cellValues, BinFormat)
      })
      contents += new MenuItem(Action("Decimal") {
        waveFormController.setWaveFormat(this, tree.selection.cellValues, DecFormat)
      })
      contents += new MenuItem(Action("Hexadecimal") {
        waveFormController.setWaveFormat(this, tree.selection.cellValues, HexFormat)
      })
    }
    if (signal.isDefined && signal.get.isInstanceOf[PureSignal]) {
      val pureSignalName = signal.get.asInstanceOf[PureSignal].name
      contents += new MenuItem(Action("Show Dependency Graph") {
        waveFormController.showDependency(pureSignalName, this)
      })
    }
  }

  val tree: Tree[SelectionNode] = new Tree[SelectionNode] {
    model = treeModel
    renderer = new SignalNameRenderer(waveFormController)
    showsRootHandles = true

    protected val expansionListener: TreeExpansionListener = new TreeExpansionListener {
      override def treeExpanded(event: TreeExpansionEvent): Unit = {
        publish(SignalsChanged(tree))
      }
      override def treeCollapsed(event: TreeExpansionEvent): Unit = {
        publish(SignalsChanged(tree))
      }
    }

    peer.addTreeExpansionListener(expansionListener)

    peer.setRowHeight(DrawMetrics.WaveformVerticalSpacing)

    // Make it rearrangeable
    peer.setDragEnabled(true)
    peer.setDropMode(DropMode.ON_OR_INSERT)
    peer.setTransferHandler(new TreeTransferHandler(waveFormController))

    def isPointInNode(point: Point): Boolean = {
      val row = getClosestRowForLocation(point.x, point.y)
      if (row >= 0) {
        val rect = peer.getRowBounds(row)
        rect.contains(point)
      } else {
        false
      }
    }

    listenTo(mouse.clicks)
    listenTo(waveFormController)
    reactions += {
      case _: SignalsChanged =>
        tree.peer.expandPath(new TreePath(model.peer.getRoot))
      case e: MouseClicked =>
        println(s"mouse clicked in inspectionContainer ${e.clicks}")
        if (SwingUtilities.isRightMouseButton(e.peer)) {
          if (isPointInNode(e.point)) {
            val row = getClosestRowForLocation(e.point.x, e.point.y)

            if (!selection.rows.contains(row)) {
              // Right clicked in a node that isn't selected
              // Then select only the node that was right clicked
              selectRows(getClosestRowForLocation(e.point.x, e.point.y))
            }
            repaint()

            val path = tree.peer.getClosestPathForLocation(e.point.x, e.point.y)
            val peerNode = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
            val node = peerNode.getUserObject.asInstanceOf[InspectedNode]
            popupMenu(node.signal).show(inspectionContainer, e.point.x, e.point.y)
          }
        } else {
          if(e.clicks == 1) {
            if (!isPointInNode(e.point)) {
              selection.clear()
            }
          }
          else if(e.clicks == 2) {
            println(s"mouse clicked in inspectionContainer ${e.clicks}")
            tree.selection.cellValues.foreach { node =>

              waveFormController.addFromDirectoryToInspected(node, this)
            }

          }
        }
    }
  }

  val inspectionContainer = new InspectionContainer(this)

  var scale: Double = 2
  var minorTickInterval: Long = 1

  var clkMinorTickInterval: Long = 1
  var useClock: Boolean = false
  var clock: Option[ClockInfo] = None

  // initial/constructor
  setScale(10, null)

  ///////////////////////////////////////////////////////////////////////////
  // Timescale and Max Timestamp
  ///////////////////////////////////////////////////////////////////////////

  var timescale: Int = -9

  ///////////////////////////////////////////////////////////////////////////
  // Helper Functions
  ///////////////////////////////////////////////////////////////////////////
  def xCoordinateToTimestamp(x: Int): Long = (x / scale).toLong
  def timestampToXCoordinate(timestamp: Long): Int = (timestamp * scale).toInt

  ///////////////////////////////////////////////////////////////////////////
  // Signals
  ///////////////////////////////////////////////////////////////////////////
  def addFromDirectoryToInspected(node: SelectionNode, source: Component): Unit = {
    treeModel.insertUnder(RootPath, node, treeModel.getChildrenOf(RootPath).size)

    if (!waveFormDataMap.contains(node)) {
      node match {
        case SelectionSignal(symbol, _) =>
          val tester = TreadleController.tester.get
          val wv = tester.allWaveformValues

          Util.toValueChange(wv, initializing = true).foreach { case (symbol, transitions) =>
            if (!symbol.name.contains("/")) {
              val waveform = if (transitions.nonEmpty) Some(new Waveform(transitions)) else None
              val sortGroup = 100

              val signal = new PureSignal(symbol.name, waveform, sortGroup)
              waveFormDataMap(node) = signal
              waveDisplaySettings(node) = WaveDisplaySetting()
            }
          }
          case _ =>
          }
      }

    publish(SignalsChanged(inspectionContainer.selectionComponent)) // TODO: Rename to NodesChanged
    publish(SignalsChanged(source)) // TODO: Rename to NodesChanged
  }

  def addGroup(): Unit = {
    val node = SelectionGroup("New Group")
    treeModel.insertUnder(RootPath, node, treeModel.getChildrenOf(RootPath).size)
  }

  // Removes all selected signals, selected groups, and children of selected groups
  // TODO: change type of paths?
  def removeSelectedSignals(source: Component, paths: Iterator[Tree.Path[SelectionNode]]): Unit = {
    paths.foreach { path =>
      treeModel.remove(path)
    }
    publish(SignalsChanged(source))
  }

  def moveSignals(source: Component): Unit = {
    publish(SignalsChanged(source))
  }

  def loadMoreWaveformValues(): Unit = {
    TreadleController.tester match {
      case Some(t) =>
        val clk = t.clockInfoList.head
        //          val wv = t.waveformValues(startCycle = ((getMaxTimeStamp - clk.initialOffset) / clk.period + 1).toInt)
        val wv = t.waveformValues()
        Util.toValueChange(wv, initializing = false).foreach {
          case (symbol, waveform) =>
            val node = SelectionSignal(symbol)
            if (waveFormDataMap.contains(node)) {
              waveFormDataMap(node).asInstanceOf[PureSignal].addNewValues(waveform)
            }
            else {
              waveFormDataMap(node) = new PureSignal(node.name, Some(new Waveform(waveform)), 0)
            }
        }

      case None =>
    }

    inspectionContainer.zoomToEnd(inspectionContainer.waveComponent)
    inspectionContainer.waveComponent.repaint()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Wave Display Format
  ///////////////////////////////////////////////////////////////////////////
  def setWaveFormat(source: Component, nodes: Iterator[SelectionNode], format: Format): Unit = {
    nodes.foreach {
      case node: WaveSignal =>
        node.format = format
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
    markers  += Marker(nextMarkerId, description, timestamp)
    markers.sortBy(_.timestamp)

    nextMarkerId += 1
    publish(MarkerChanged(timestamp, null))
  }

  // Returns index of element that matches
  def getMarkerAtTime(timestamp: Long): Int = {
    markers.reverse.indexWhere(timestamp >= _.timestamp) match {
      case i if i >= 0 => markers.size - 1 - i
      case _ => 0
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