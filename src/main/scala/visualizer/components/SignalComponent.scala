package visualizer.components

import java.awt.{Color, Font}

import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.models._

import scala.swing._
import scala.swing.event._
import BorderPanel.Position.Center

class SignalComponent(dataModel: DataModel, displayModel: DisplayModel, tree: Tree[InspectedNode])
  extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  add(tree, Center)
  preferredSize = new Dimension(220, 500)
  focusable = true

  def computeBounds(): Unit = {
    preferredSize = new Dimension(220, TreeHelper.viewableDepthFirstIterator(tree).size *
      DrawMetrics.WaveformVerticalSpacing)
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(displayModel)
  listenTo(keys, tree.keys)
  listenTo(mouse.clicks)
  reactions += {
    case _: SignalsChanged =>
      computeBounds()
      repaint()
    case _: WaveFormatChanged | _: CursorSet =>
      repaint()
    case KeyReleased(_, Key.BackSpace, _, _) =>
      displayModel.removeSelectedSignals(this, tree.selection.paths.iterator)
  }
}

class SignalNameRenderer(dataModel: DataModel, displayModel: DisplayModel) extends Tree.Renderer[InspectedNode] {
  private var labelBaseLine = -1
  private var valueBaseLine = 0
  val SignalNameFont = new Font("SansSerif", Font.BOLD, 10)
  val ValueFont = new Font("SansSerif", Font.PLAIN, 8)

  override def componentFor(
      owner: Tree[_],
      value: InspectedNode,
      cellInfo: companion.CellInfo
  ): Component = {
    new SignalNamePanel(value, cellInfo.isSelected)
  }

  class SignalNamePanel(node: InspectedNode, isSelected: Boolean) extends BorderPanel {
    peer.setOpaque(true)
    preferredSize = new Dimension(200, DrawMetrics.WaveformVerticalSpacing)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)

      if (labelBaseLine == -1) {
        // Initialized once
        val labelMetrics = g.getFontMetrics(SignalNameFont)
        val valueMetrics = g.getFontMetrics(ValueFont)
        labelBaseLine = labelMetrics.getAscent
        valueBaseLine = labelBaseLine + labelMetrics.getDescent +
          labelMetrics.getLeading + valueMetrics.getAscent
        val totalHeight = valueBaseLine + valueMetrics.getDescent
        val border = (DrawMetrics.WaveformVerticalSpacing - totalHeight) / 2
        labelBaseLine += border
        valueBaseLine += border
      }

      node.signal match {
        case Some(signal) =>
          // Background
          if (isSelected) g.setColor(Color.blue) else g.setColor(Color.white)
          g.fillRect(0, 0, peer.getWidth, DrawMetrics.WaveformVerticalSpacing)

          // Signal Name
          g.setFont(SignalNameFont)
          if (isSelected) g.setColor(Color.white) else g.setColor(Color.black)
          g.drawString(node.name, 1, labelBaseLine)

          // Value
          g.setFont(ValueFont)
          if (isSelected) g.setColor(Color.white) else g.setColor(Color.blue)
          val value = signal.findTransition(displayModel.cursorPosition).next().value
          val txt = signal match {
            case _: PureSignal =>
              displayModel.waveDisplaySettings(node.nodeId).dataFormat.getOrElse(DecFormat)(value.asInstanceOf[BigInt])
            case _: CombinedSignal =>
              val pair = value.asInstanceOf[Array[BigInt]]
              (pair(0).toInt, pair(1).toInt) match {
                case (0, 0) => "Not ready"
                case (1, 1) => "Ready"
                case _ => "Waiting"
              }
            case _ => ""
          }
          g.drawString(txt, 1, valueBaseLine)
        case _ =>
      }
    }
  }
}