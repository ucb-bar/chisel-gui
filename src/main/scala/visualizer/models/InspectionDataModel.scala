package visualizer.models

import scalaswingcontrib.tree._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Transition(timestamp: Long, value: BigInt)

case class Waveform(name: String, transitions: ArrayBuffer[Transition]) {

  // Return iterator starting from the transition at the timestamp or the
  // transition before the timestamp. If timestamp is before the first transition,
  // retun the first transition
  def findTransition(timestamp: Long): Iterator[Transition] = {
    def search(low: Int = 0, high: Int = transitions.size - 1): ArrayBuffer[Transition] = {
      val mid = (low + high)/2

      if (low > high) {
        if (low == 0) transitions else transitions.drop(low - 1)
      } else if (transitions(mid).timestamp == timestamp) {
        transitions.drop(mid)
      } else if (transitions(mid).timestamp > timestamp) {
        search(low, mid - 1)
      } else {
        search(mid + 1, high)
      }
    }
    search().iterator
  }
}

class InspectedNode(val nodeId: Int, val waveId: Int, val name: String) {
  def copy: InspectedNode = {
    InspectedNode(waveId, name)
  }
}

object InspectedNode {
  private var nextNodeId = 0

  def apply(waveId: Int, name: String): InspectedNode = {
    val retVal = new InspectedNode(nextNodeId, waveId, name)
    nextNodeId += 1
    retVal
  }
}

case class DirectoryNode(waveId: Int, name: String) {
  def toInspected: InspectedNode = {
    InspectedNode(waveId, name)
  }
}

class InspectionDataModel {

  // TODO: remove allWaves
  val allWaves = new mutable.HashMap[String, Waveform]

  // A signal's waveform will be stored here. Avoids duplicating data if signal is going to be drawn 2+ more times.
  // Choosing a map over an arraybuffer because artificial signals (think splitting bundles) may be
  // added and then removed, while the key/index cannot be changed for other signals (w/o significant cost)
  val waveforms = new mutable.HashMap[Int, Waveform]


  val temporaryNode = DirectoryNode(-1, "root")
  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel(temporaryNode)(_ => Seq.empty[DirectoryNode])
  val RootPath = Tree.Path.empty[DirectoryNode]
  val tree = new Tree[DirectoryNode] {
    model = directoryTreeModel
    renderer = Tree.Renderer(_.name)
    showsRootHandles = true
  }


  var maxTimestamp: Long = 0

  def updateMaxTimestamp: Unit = {
//    maxTimestamp = allWaves.values.map { w => w.transitions(w.transitions.size - 1).timestamp }.max
    maxTimestamp = waveforms.values.map{ w => w.transitions(w.transitions.size - 1).timestamp }.max
    println(s"new max timestamp: $maxTimestamp")
  }


  var timescale: Int = -9



}