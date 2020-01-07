package visualizer.components

import firrtl.ir.FileInfo
import javax.swing.event.{TreeExpansionEvent, TreeExpansionListener}
import javax.swing.tree.{DefaultMutableTreeNode, TreePath}
import javax.swing.{BorderFactory, DropMode, SwingUtilities}
import scalaswingcontrib.tree.Tree
import visualizer.models._
import visualizer.{DrawMetrics, SignalsChanged, TreadleController}

import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.MouseClicked
import scala.sys.process._
import scala.util.matching.Regex

/** This manages the signals selected for the waveform display
  * signals can be added, moved, removed, and have their display formats changed
  *
  * @param dataModel    underlying data model
  * @param displayModel model used by tree to describe selected signals
  */
class InspectionContainer(dataModel: DataModel, displayModel: DisplayModel) extends BorderPanel {

  val SourceInfoPattern: Regex = """([^ ]*) (\d*).*""".r

  // Popup menu when a signal name is right-clicked
  private def popupMenu(signal: Option[Signal[_]]): PopupMenu = new PopupMenu {
    contents += new Menu("Data Format") {
      contents += new MenuItem(Action("Binary") {
        displayModel.setWaveFormat(this, tree.selection.cellValues, BinFormat)
      })
      contents += new MenuItem(Action("Decimal") {
        displayModel.setWaveFormat(this, tree.selection.cellValues, DecFormat)
      })
      contents += new MenuItem(Action("Hexadecimal") {
        displayModel.setWaveFormat(this, tree.selection.cellValues, HexFormat)
      })
    }
    signal match {
      case Some(pureSignal: PureSignal) =>
        contents += new MenuItem(Action("Add Driving Signals") {
          TreadleController.loadDrivingSignals(pureSignal)
        })

        pureSignal.symbolOpt.foreach { symbol =>
          symbol.info match {
            case s: FileInfo =>
              s.info.string match {
                case SourceInfoPattern(file, line) =>
                  TreadleController.sourceInfoMap.get(file).foreach { targetFile =>
                    val command = Seq(
                      "/Applications/IntelliJ IDEA.app/Contents/MacOS/idea",
                      "--line",
                      s"$line",
                      s"$targetFile"
                    )

                    println(command.mkString(" "))
                    contents += new MenuItem(Action(s"Jump to source $file : $line") {
                      command.!!
                    })
                  }
                case _ =>
              }
            case _ =>
          }
        }
      case _ =>
    }
  }

  val tree: Tree[GenericTreeNode] = new Tree[GenericTreeNode] {
    model = displayModel.treeModel
    renderer = new SignalNameRenderer(dataModel, displayModel)
    showsRootHandles = true

    protected val expansionListener: TreeExpansionListener = new TreeExpansionListener {
      override def treeExpanded(event: TreeExpansionEvent): Unit = {
        publish(SignalsChanged(InspectionContainer.this.tree))
      }

      override def treeCollapsed(event: TreeExpansionEvent): Unit = {
        publish(SignalsChanged(InspectionContainer.this.tree))
      }
    }

    peer.addTreeExpansionListener(expansionListener)

    peer.setRowHeight(DrawMetrics.WaveformVerticalSpacing)

    // Make it re-arrangeable
    peer.setDragEnabled(true)
    peer.setDropMode(DropMode.ON_OR_INSERT)
    peer.setTransferHandler(new TreeTransferHandler(displayModel))

    def isPointInNode(point: Point): Boolean = {
      val row = getClosestRowForLocation(point.x, point.y)
      if (row >= 0) {
        val rect = peer.getRowBounds(row)
        rect.contains(point)
      } else {
        false
      }
    }

    def handleMouseClick(e: MouseClicked): Unit = {
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
          val node = peerNode.getUserObject.asInstanceOf[GenericTreeNode]
          node match {
            case waveFormNode: WaveFormNode =>
              popupMenu(Some(waveFormNode.signal)).show(this, e.point.x, e.point.y)
            case _ =>
          }
        }
      } else {
        if (e.clicks == 1) {
          if (!isPointInNode(e.point)) {
            selection.clear()
          }
        } else if (e.clicks == 2) {
          println(s"mouse clicked in inspectionContainer ${e.clicks}")
          tree.selection.cellValues.foreach { node =>
            displayModel.addFromDirectoryToInspected(node, this)
          }
        }
      }
    }

    listenTo(mouse.clicks)
    listenTo(displayModel)
    reactions += {
      case _: SignalsChanged =>
        tree.peer.expandPath(new TreePath(model.peer.getRoot))
      case e: MouseClicked =>
        handleMouseClick(e)
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  preferredSize = new Dimension(500, 700)

  val timelineComponent = new TimelineComponent(dataModel, displayModel)
  val waveComponent = new WaveComponent(dataModel, displayModel, tree)
  val signalComponent = new SignalComponent(dataModel, displayModel, tree)

  val signalScrollPane: ScrollPane = new ScrollPane(signalComponent) {
    minimumSize = new Dimension(150, 300)
    border = BorderFactory.createEmptyBorder()

    verticalScrollBar.unitIncrement = 16

    // prevents apple trackpad jittering
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
  }
  val waveScrollPane: ScrollPane = new ScrollPane(waveComponent) {
    preferredSize = new Dimension(550, 700)
    horizontalScrollBar.unitIncrement = 16
    verticalScrollBar.unitIncrement = 16
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
    verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    columnHeaderView = timelineComponent
  }
  signalScrollPane.verticalScrollBar.peer.setModel(
    waveScrollPane.verticalScrollBar.peer.getModel
  )

  val splitPane: SplitPane = new SplitPane(
    Orientation.Vertical,
    new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(timelineComponent.preferredSize.height)
      contents += signalScrollPane
    },
    waveScrollPane
  ) {
    border = BorderFactory.createEmptyBorder()
  }
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

  /**
    * move wave view to end, keeping the current scale
    * @param source component to scroll
    */
  def zoomToEnd(source: Component): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect
    val maxTimestamp = dataModel.maxTimestamp

    val clockTickWidth = oldVisibleRect.width / displayModel.scale

    val minTimestamp = (maxTimestamp - clockTickWidth).max(0)

    val centerTimestamp = (maxTimestamp - minTimestamp) / 2 + minTimestamp

    val centerX = (centerTimestamp * displayModel.scale).toInt

    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = centerX - newVisibleRect.width / 2
    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }

  def removeSignals(source: Component): Unit = {
    displayModel.removeSelectedSignals(source, tree.selection.paths.iterator)
  }

  def goToEnd(source: Component, steps: Int): Unit = {
    val oldVisibleRect = waveComponent.peer.getVisibleRect

    val newVisibleRect = waveComponent.peer.getVisibleRect
    newVisibleRect.x = (oldVisibleRect.x + steps / displayModel.scale).toInt

    waveComponent.peer.scrollRectToVisible(newVisibleRect)
  }
}
