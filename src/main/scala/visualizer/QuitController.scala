// See LICENSE for license details.

package visualizer

import visualizer.controllers.WaveFormController
import visualizer.models.{TreeHelper, WaveGroup, WaveSignal}

class QuitController(waveFormController: WaveFormController) {
  def saveWaveformInfo() {
    TreeHelper.viewableDepthFirstIterator(waveFormController.tree).foreach {
      case signal: WaveSignal =>
        // serialize and output
      case group: WaveGroup =>
      // serialize and output
    }
  }

  def readWaveformInfo(fileName: String): Unit = {
    // read the file
    // parse the json

  }
}
