// See LICENSE for license details.

package visualizer

import scalaswingcontrib.tree.Tree
import treadle.executable.SymbolTable
import visualizer.controllers.WaveFormController
import visualizer.models.{SelectionNode, TreeHelper, WaveGroup, WaveSignal}

class QuitController(waveFormController: WaveFormController) {
  def saveWaveformInfo(tree: Tree[SelectionNode]) {
    TreeHelper.viewableDepthFirstIterator(waveFormController.tree).foreach {
      case signal: WaveSignal =>
        // serialize and output
      case group: WaveGroup =>
      // serialize and output
      case _ =>
        // should not be any thing here, throw exception
    }
  }

  /**
    * populate tree from file
    * @param fileName
    * @param tree an empty tree to be populated
    * @param symbolTable use this to check if
    */
  def readWaveformInfo(fileName: String, tree: Tree[SelectionNode], symbolTable: SymbolTable): Unit = {
    // read the file
    // parse the json

  }
}
