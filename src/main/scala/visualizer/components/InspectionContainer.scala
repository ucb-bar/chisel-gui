package visualizer.components

import javax.swing.{DropMode, SwingUtilities}
import javax.swing.event.{TreeExpansionEvent, TreeExpansionListener}
import scalaswingcontrib.event.{TreeCollapsed, TreeExpanded}
import scalaswingcontrib.tree.Tree
import visualizer.DrawMetrics
import visualizer.models._

import scala.swing._
import BorderPanel.Position._
import scala.swing.event.MouseClicked

class InspectionContainer(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel) extends BorderPanel {

  // Popup menu
  private lazy val popupMenu = new PopupMenu {
    contents += new Menu("Data Format") {
      contents += new MenuItem(Action("Binary") {
        tree.selection.cellValues.foreach{node =>
          displayModel.setWaveFormat(this, node.signalId, BinFormat)
        }
      })
      contents += new MenuItem(Action("Decimal") {
        tree.selection.cellValues.foreach{node =>
          displayModel.setWaveFormat(this, node.signalId, DecFormat)
        }
      })
      contents += new MenuItem(Action("Hexadecimal") {
        tree.selection.cellValues.foreach{node =>
          displayModel.setWaveFormat(this, node.signalId, HexFormat)
        }
      })
    }
  }

  val tree: Tree[InspectedNode] = new Tree[InspectedNode] {
    model = displayModel.treeModel
    renderer = new SignalNameRenderer(dataModel, displayModel)
    showsRootHandles = true

    protected val expansionListener: TreeExpansionListener = new TreeExpansionListener {
      override def treeExpanded(event: TreeExpansionEvent): Unit = {
        publish(TreeExpanded[InspectedNode](InspectionContainer.this.tree, treePathToPath(event.getPath)))
      }
      override def treeCollapsed(event: TreeExpansionEvent): Unit = {
        publish(TreeCollapsed[InspectedNode](InspectionContainer.this.tree, treePathToPath(event.getPath)))
      }
    }

    peer.addTreeExpansionListener(expansionListener)

    peer.setRowHeight(DrawMetrics.WaveformVerticalSpacing)

    // Make it rearrangeable
    peer.setDragEnabled(true)
    peer.setDropMode(DropMode.ON_OR_INSERT)
    peer.setTransferHandler(new TreeTransferHandler(displayModel))

    def isPointInNode(point: Point): Boolean = {
      val row = getClosestRowForLocation(point.x, point.y)
      val rect = peer.getRowBounds(row)
      rect.contains(point)
    }

    listenTo(mouse.clicks)
    reactions += {
      case e: MouseClicked =>
        if (SwingUtilities.isRightMouseButton(e.peer)) {
          if (isPointInNode(e.point)) {
            val row = getClosestRowForLocation(e.point.x, e.point.y)

            if (!selection.rows.contains(row)) {
              // Right clicked in a node that isn't selected
              // Then select only the node that was right clicked
              selectRows(getClosestRowForLocation(e.point.x, e.point.y))
            }

            repaint()
            popupMenu.show(this, e.point.x, e.point.y)
          }
        } else {
          if (!isPointInNode(e.point)) {
            selection.clear()
          }
        }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val timelineComponent = new TimelineComponent(dataModel, displayModel)
  val waveComponent = new WaveComponent(dataModel, displayModel, tree)
  val signalComponent = new SignalComponent(dataModel, displayModel, tree)

  val signalScrollPane: ScrollPane = new ScrollPane(signalComponent) {
    verticalScrollBar.unitIncrement = 16
  }
  val waveScrollPane: ScrollPane = new ScrollPane(waveComponent) {
    horizontalScrollBar.unitIncrement = 16
    verticalScrollBar.unitIncrement = 16
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    columnHeaderView = timelineComponent
  }
  signalScrollPane.verticalScrollBar.peer.setModel(
    waveScrollPane.verticalScrollBar.peer.getModel
  )

  val splitPane = new SplitPane(Orientation.Vertical,
    new BorderPanel {
      add(Swing.VStrut(timelineComponent.preferredSize.height), North)
      add(signalScrollPane, Center)
      add(Swing.VStrut(waveScrollPane.horizontalScrollBar.preferredSize.height), South)
    },
    waveScrollPane
  )
  add(splitPane, Center)


  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  def setScaleKeepCentered(newScale: Double, source: Component): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect
    val centerTimestamp = ((oldVisibleRect.x + oldVisibleRect.width / 2) / displayModel.scale).toLong

    displayModel.setScale(newScale, source)

    val centerX = (centerTimestamp * newScale).toInt
    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = centerX - newVisibleRect.width / 2
    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }

  def zoomIn(source: Component): Unit = {
    setScaleKeepCentered(displayModel.scale * 1.25, source)
  }

  def zoomOut(source: Component): Unit = {
    setScaleKeepCentered(displayModel.scale * 0.8, source)
  }

  def removeSignals(source: Component): Unit = {
    displayModel.removeSelectedSignals(source, tree.selection.paths.iterator)
  }
}
