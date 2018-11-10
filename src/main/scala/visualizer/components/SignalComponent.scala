package visualizer.components

import java.awt.{Color, Font}

import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.controllers.WaveFormController
import visualizer.models._

import scala.swing.BorderPanel.Position.Center
import scala.swing._
import scala.swing.event._

/**
  * This is the organizing list of the users selected signals.
  * @param waveFormController the controller for this component
  */
class SignalComponent(waveFormController: WaveFormController) extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  val tree: Tree[SelectionNode] = waveFormController.tree
  add(tree, Center)
  focusable = true

  def computeBounds(): Unit = {
    preferredSize = new Dimension(125, TreeHelper.viewableDepthFirstIterator(tree).size *
      DrawMetrics.WaveformVerticalSpacing)
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(waveFormController)
  listenTo(keys, tree.keys)
  listenTo(mouse.clicks)
  reactions += {
    case _: SignalsChanged =>
      computeBounds()
      repaint()
    case _: WaveFormatChanged | _: CursorSet =>
      repaint()
    case m: MouseButtonEvent =>
      println(s"Mouse clicked ${m.clicks}")
    case KeyReleased(_, Key.BackSpace, _, _) =>
      waveFormController.removeSelectedSignals(this, tree.selection.paths.iterator)
  }
}

class SignalNameRenderer(waveFormController: WaveFormController) extends Tree.Renderer[SelectionNode] {
  private var labelBaseLine = -1
  private var valueBaseLine = 0
  val SignalNameFont = new Font("SansSerif", Font.BOLD, 10)
  val ValueFont = new Font("SansSerif", Font.PLAIN, 8)

  def componentFor(
      owner: Tree[_],
      value: SelectionNode,
      cellInfo: companion.CellInfo
  ): Component = {
    new SignalNamePanel(value, cellInfo.isSelected)
  }

  class SignalNamePanel(node: SelectionNode, isSelected: Boolean) extends BorderPanel {
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

      node match {
        case waveSignal: WaveSignal =>
          val waveform = waveSignal.waveform
          val iterator = waveform.findTransition(waveFormController.cursorPosition)
          if(iterator.hasNext) {
            val value    = iterator.next().value
            waveSignal.format(value.asInstanceOf[BigInt])
          }

        case waveGroup: WaveGroup =>
          //TODO: make this work with groups that are based on composition of child waveforms
//          val waveform = waveGroup.waveform
//          val value = waveform.findTransition(waveFormController.cursorPosition).next().value
//
//          val txt = signal match {
//            case _: PureSignal if value.asInstanceOf[BigInt] != null =>
//              waveFormController.waveDisplaySettings(node).dataFormat.getOrElse(DecFormat)(value.asInstanceOf[BigInt])
//            case _: CombinedSignal =>
//              val pair = value.asInstanceOf[Array[BigInt]]
//              if (pair != null) {
//                (pair(0).toInt, pair(1).toInt) match {
//                  case (0, 0) => "Not ready"
//                  case (1, 1) => "Ready"
//                  case _ => "Waiting"
//                }
//              } else {
//                ""
//              }
//            case _ => ""
//          }
//          g.drawString(txt, 1, valueBaseLine)
        case _ =>
          // Node is a group
          // Background
          if (isSelected) g.setColor(Color.blue) else g.setColor(Color.white)
          g.fillRect(0, 0, peer.getWidth, DrawMetrics.WaveformVerticalSpacing)

          // Group Name
          g.setFont(SignalNameFont) // TODO: Rename SignalNameFont
          if (isSelected) g.setColor(Color.white) else g.setColor(Color.black)
          g.drawString(node.name, 1, labelBaseLine)
      }
    }
  }
}