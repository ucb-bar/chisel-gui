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
