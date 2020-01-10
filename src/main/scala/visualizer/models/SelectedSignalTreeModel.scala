// See README.md for license details.

package visualizer.models

import scala.swing._
import scalaswingcontrib.tree._

class SelectedSignalTreeModel extends InternalTreeModel[GenericTreeNode] {

  def insert(node: GenericTreeNode, insertionPoint: InsertionPoint): Unit = {}

}

case class InsertionPoint(path: Tree.Path[GenericTreeNode], index: Int) {
  def next: InsertionPoint = copy(index = index + 1)
}
