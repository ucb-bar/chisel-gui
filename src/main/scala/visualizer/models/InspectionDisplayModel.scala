package visualizer.models

import scalaswingcontrib.tree._

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scala.swing.event.ActionEvent

class InspectionDisplayModel extends Publisher {
  // TODO: change to tree rather than arraybuffer
  val inspectedWaves = new ArrayBuffer[Waveform]()

  // Replacing inspectedWaves with a TREE
  val temporaryNode = TreeNode("temp", -2)
  val displayTreeModel : InternalTreeModel[TreeNode] = InternalTreeModel(temporaryNode)(_ => Seq.empty[TreeNode])
  val RootPath = Tree.Path.empty[TreeNode]
  val tree = new Tree[TreeNode] {
    model = displayTreeModel
    renderer = Tree.Renderer(_.name) // TODO: use custom renderer to adjust height of row and include value at cursor
    showsRootHandles = true
  }










  var scale : Double = 2

  def addSignal(node : TreeNode, source : Component) = {
    displayTreeModel.insertUnder(RootPath, node, displayTreeModel.getChildrenOf(RootPath).size)
    publish(SignalsAdded(source))
  }

  def addModule(moduleNode : TreeNode, source : Component) : Unit = {
    displayTreeModel.insertUnder(RootPath, moduleNode, displayTreeModel.getChildrenOf(RootPath).size)

    publish(SignalsAdded(source))
  }

  def zoomIn(source : Component) : Unit = {
    scale *= 1.25
    publish(ScaleChanged(source))
  }

  def zoomOut(source : Component) : Unit = {
    scale *= 0.8
    publish(ScaleChanged(source))
  }

}

object InspectionDisplayModel {
  trait Listener {
    def wavesAdded
  }
}

case class Marker(id : Int, description: String, timestamp : Long)

// Events
case class SignalsAdded(override val source: Component) extends ActionEvent(source)
case class ScaleChanged(override val source: Component) extends ActionEvent(source)