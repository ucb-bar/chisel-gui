package visualizer.models

import scalaswingcontrib.tree._

class DataModel {
  val directoryTreeModel: InternalTreeModel[DirectoryNode] = InternalTreeModel.empty[DirectoryNode]
  val RootPath: Tree.Path[DirectoryNode] = Tree.Path.empty[DirectoryNode]

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