package visualizer.models

import javax.swing.DropMode
import javax.swing.event.{TreeExpansionEvent, TreeExpansionListener}
import scalaswingcontrib.event.{TreeCollapsed, TreeExpanded}
import scalaswingcontrib.tree.Tree.Path

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scalaswingcontrib.tree._
import visualizer._

class InspectionDisplayModel extends Publisher {
  val temporaryNode = InspectedNode(-2, "temp")
  val displayTreeModel: InternalTreeModel[InspectedNode] = InternalTreeModel(temporaryNode)(_ => Seq.empty[InspectedNode])

  val RootPath: Tree.Path[InspectedNode] = Tree.Path.empty[InspectedNode]
  val tree: Tree[InspectedNode] = new Tree[InspectedNode] {
    model = displayTreeModel
    renderer = Tree.Renderer(_.name) // TODO: use custom renderer to adjust height of row and include value at cursor
    showsRootHandles = true

    protected val expansionListener: TreeExpansionListener = new TreeExpansionListener {
      override def treeExpanded(event: TreeExpansionEvent): Unit = {
        publish(TreeExpanded[InspectedNode](InspectionDisplayModel.this.tree, treePathToPath(event.getPath)))
      }
      override def treeCollapsed(event: TreeExpansionEvent): Unit = {
        publish(TreeCollapsed[InspectedNode](InspectionDisplayModel.this.tree, treePathToPath(event.getPath)))
      }
    }

    peer.addTreeExpansionListener(expansionListener)

    peer.setRowHeight(DrawMetrics.WaveformVerticalSpacing + DrawMetrics.WaveformHeight)

    // Make it rearrangeable
    peer.setDragEnabled(true)
    peer.setDropMode(DropMode.ON_OR_INSERT)
    // Is there a better way to pass the display model to the transfer handler?
    // Transfer handler could be nested within display model?
    peer.setTransferHandler(new TreeTransferHandler(InspectionDisplayModel.this))
  }

  var scale: Double = 2
  var minorTickInterval: Long = 1

  var clkMinorTickInterval: Long = 1
  var useClock: Boolean = false
  var clock: Option[Clock] = None

  // initial/constructor
  setScale(10)


  ///////////////////////////////////////////////////////////////////////////
  // Signals
  ///////////////////////////////////////////////////////////////////////////
  def addSignal(node: DirectoryNode, source: Component): Unit = {
    displayTreeModel.insertUnder(RootPath, node.toInspected, displayTreeModel.getChildrenOf(RootPath).size)
    publish(SignalsChanged(source))
  }

  def addModule(moduleNode: DirectoryNode, source: Component): Unit = {
    displayTreeModel.insertUnder(RootPath, moduleNode.toInspected, displayTreeModel.getChildrenOf(RootPath).size)

    publish(SignalsChanged(source))
  }

  def moveSignals(source: Component): Unit = {
    publish(SignalsChanged(source))
  }

  ///////////////////////////////////////////////////////////////////////////
  // Scale
  ///////////////////////////////////////////////////////////////////////////
  def setScale(newScale: Double): Unit = {
    scale = newScale
    val x = math.pow(10, math.ceil(math.log10(DrawMetrics.MinMinorTickHSpace / scale))).toLong
    minorTickInterval = if (x <= 0) 1 else x
  }

  def zoomIn(source: Component): Unit = {
    setScale(scale * 1.25)
    publish(ScaleChanged(source))
  }

  def zoomOut(source: Component): Unit = {
    setScale(scale * 0.8)
    publish(ScaleChanged(source))
  }

  ///////////////////////////////////////////////////////////////////////////
  // Timeline
  ///////////////////////////////////////////////////////////////////////////
  def setClock(waveId: Long): Unit = {
    // Verify wave matches a clock?
    clock = Some(Clock(6, 10))
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
  // Random tree helper
  ///////////////////////////////////////////////////////////////////////////
  def viewableDepthFristIterator(
      tree: Tree[InspectedNode] = tree,
      treeModel: TreeModel[InspectedNode] = displayTreeModel): Iterator[InspectedNode] = new Iterator[InspectedNode] {

    var openNodes: Iterator[Path[InspectedNode]] = treeModel.roots.map(Path(_)).iterator

    def hasNext: Boolean = openNodes.nonEmpty
    def next(): InspectedNode = if (openNodes.hasNext) {
      val path = openNodes.next()
      pushChildren(path)
      path.last
    } else throw new NoSuchElementException("No more items")

    def pushChildren(path: Path[InspectedNode]): Unit = {
      if (tree.isExpanded(path)) {
        val open = openNodes
        openNodes = treeModel.getChildPathsOf(path).toIterator ++ open
      }
    }
  }
}

case class Marker(id: Int, var description: String, timestamp: Long)
case class Clock(startTime: Long, cycleDuration: Long)