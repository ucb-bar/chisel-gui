package visualizer.components

import java.awt
import java.awt.{Color, Font}

import javax.swing.JTree
import javax.swing.tree.{DefaultMutableTreeNode, TreeCellRenderer}
import scalaswingcontrib.tree.Tree
import visualizer._
import visualizer.models._

import scala.swing._
import BorderPanel.Position.Center

class SignalComponent(dataModel: InspectionDataModel, displayModel: InspectionDisplayModel)
  extends BorderPanel {

  val signalView = displayModel.tree
  signalView.renderer = new SignalNameRenderer(dataModel, displayModel)

  add(signalView, Center)


  listenTo(displayModel)
  reactions += {
    case e: SignalsAdded => signalsAdded
  }
  def signalsAdded: Unit = { }

}

class SignalNameRenderer(
    dataModel: InspectionDataModel, displayModel: InspectionDisplayModel)
    extends Tree.Renderer[TreeNode] {
  var currentSignalNode = TreeNode("asdf", -10)
  var currentSignalIsSelected = false

  var labelBaseLine = -1
  var valueBaseLine = 0
  val SignalNameFont = new Font("SansSerif", Font.BOLD, 10)
  val ValueFont = new Font("SansSerif", Font.PLAIN, 8)

//  override def getTreeCellRendererComponent(
//      tree: JTree,
//      value: scala.Any,
//      sel: Boolean,
//      expanded: Boolean,
//      leaf: Boolean,
//      row: Int,
//      hasFocus: Boolean): awt.Component = {
//
//    currentSignalNode = value.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[TreeNode]
//    currentSignalIsSelected = sel
//    this.peer
//  }

  override def componentFor(
      owner: Tree[_],
      value: TreeNode,
      cellInfo: companion.CellInfo): Component = {

    currentSignalNode = value
    currentSignalIsSelected = cellInfo.isSelected
    new SignalNamePanel
  }

  class SignalNamePanel extends BorderPanel {
    peer.setOpaque(true)
    preferredSize = new Dimension(200, DrawMetrics.WaveformHeight + DrawMetrics.WaveformVerticalSpacing)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)

      if (currentSignalNode.id >= 0) { // paint only signals, not groups

        if (labelBaseLine == -1) {
          // Initialized once
          val labelMetrics = g.getFontMetrics(SignalNameFont)
          val valueMetrics = g.getFontMetrics(ValueFont)
          labelBaseLine = labelMetrics.getAscent
          valueBaseLine = labelBaseLine + labelMetrics.getDescent +
            labelMetrics.getLeading + valueMetrics.getAscent
          val totalHeight = valueBaseLine + valueMetrics.getDescent
          val border = (DrawMetrics.WaveformVerticalSpacing + DrawMetrics.WaveformHeight - totalHeight) / 2
          labelBaseLine += border
          valueBaseLine += border
        }

        // Change color depending on if currentSignalIsSelected

        if (currentSignalIsSelected) {
          g.setColor(Color.blue)
        } else {
          g.setColor(Color.white)
        }
        g.fillRect(0, 0, peer.getWidth, DrawMetrics.WaveformVerticalSpacing + DrawMetrics.WaveformHeight)

        g.setFont(SignalNameFont)
        if (currentSignalIsSelected) g.setColor(Color.white) else g.setColor(Color.black)
        g.drawString(currentSignalNode.name, 1, labelBaseLine)

        g.setFont(ValueFont)
        if (currentSignalIsSelected) g.setColor(Color.white) else g.setColor(Color.blue)
        val t = dataModel.waveforms(currentSignalNode.id).findTransition(displayModel.cursorPosition).next()
        g.drawString(t.value.toString, 1, valueBaseLine)
      }

    }
  }
}