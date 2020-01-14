package visualizer.components

import java.awt.{Color, Font}

import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.config.DrawMetrics
import visualizer.models._

import scala.swing.BorderPanel.Position.Center
import scala.swing._
import scala.swing.event._

class SelectedSignalPanel(dataModel: DataModel, selectedSignalModel: SelectedSignalModel, tree: Tree[GenericTreeNode])
  extends BorderPanel {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  add(tree, Center)
  focusable = true

  def computeBounds(): Unit = {
    preferredSize = new Dimension(600,
      TreeHelper.viewableDepthFirstIterator(tree).size *
        DrawMetrics.WaveformVerticalSpacing)
    revalidate()
  }

  ///////////////////////////////////////////////////////////////////////////
  // Controller
  ///////////////////////////////////////////////////////////////////////////
  listenTo(selectedSignalModel)
  listenTo(keys, tree.keys)

  reactions += {
    case _: SignalsChanged =>
      computeBounds()
      repaint()
    case _: WaveFormatChanged | _: CursorSet =>
      repaint()
    case KeyReleased(_, Key.BackSpace, _, _) =>
      selectedSignalModel.removeSelectedSignals(this, tree.selection.paths.iterator)
  }
}

/** Old way to render a selected signal
  *
  * @param dataModel           the data
  * @param selectedSignalModel the model
  */
//TODO: Remove this after a bit of usage of the new way, see InspectionContainer#show
class SignalNameRenderer(dataModel: DataModel, selectedSignalModel: SelectedSignalModel)
  extends Tree.Renderer[GenericTreeNode] {
  private var labelBaseLine = -1
  private var valueBaseLine = 0
  val SignalNameFont = new Font("SansSerif", Font.BOLD, 10)
  val ValueFont = new Font("SansSerif", Font.PLAIN, 8)

  override def componentFor(
                             owner: Tree[_],
                             value: GenericTreeNode,
                             cellInfo: companion.CellInfo
                           ): Component = {
    new SignalNamePanel(value, cellInfo.isSelected)
  }

  class SignalNamePanel(node: GenericTreeNode, isSelected: Boolean) extends BorderPanel {
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

      node match {
        case _: DirectoryNode =>
          // Node is a group
          // Background
          if (isSelected) g.setColor(Color.blue) else g.setColor(Color.white)
          g.fillRect(0, 0, peer.getWidth, DrawMetrics.WaveformVerticalSpacing)

          // Group Name
          g.setFont(SignalNameFont) // TODO: Rename SignalNameFont
          if (isSelected) g.setColor(Color.white) else g.setColor(Color.black)
          g.drawString(node.name, 1, labelBaseLine)

        case signalTreeNode: SignalTreeNode =>
          dataModel.nameToSignal.get(node.name) match {
            case Some(signal) if signal.waveform.isDefined =>
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
              val value = signal.waveform.get.findTransition(selectedSignalModel.cursorPosition).next().value
              val txt = signal match {
                case pureSignal: PureSignal if value.asInstanceOf[BigInt] != null =>
                  val setting = selectedSignalModel.waveDisplaySettings(pureSignal.name)
                  setting.dataFormat.getOrElse(DecFormat)(value.asInstanceOf[BigInt])
                case _: CombinedSignal =>
                  val pair = value.asInstanceOf[Array[BigInt]]
                  if (pair != null) {
                    (pair(0).toInt, pair(1).toInt) match {
                      case (0, 0) => "Not ready"
                      case (1, 1) => "Ready"
                      case _ => "Waiting"
                    }
                  } else {
                    ""
                  }
                case _ => ""
              }
              g.drawString(txt, 1, valueBaseLine)
            case None =>
          }

        case _ =>
      }
    }
  }
}
