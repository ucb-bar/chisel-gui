package visualizer.models

import scalaswingcontrib.tree._

class DataModel {
  val temporaryNode = DirectoryNode("root", None)
  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel(temporaryNode)(_ => Seq.empty[DirectoryNode])
  val RootPath: Tree.Path[DirectoryNode] = Tree.Path.empty[DirectoryNode]
  val tree: Tree[DirectoryNode] = new Tree[DirectoryNode] {
    model = directoryTreeModel
    renderer = Tree.Renderer(_.name)
    showsRootHandles = true
  }

  var maxTimestamp: Long = 0
  def updateMaxTimestamp(): Unit = {
    maxTimestamp = 0
    directoryTreeModel.depthFirstIterator.foreach { node =>
      node.signal match {
        case Some(signal) => maxTimestamp = math.max(maxTimestamp, signal.waveform.last.timestamp)
        case None =>
      }
    }
  }

  var timescale: Int = -9
}