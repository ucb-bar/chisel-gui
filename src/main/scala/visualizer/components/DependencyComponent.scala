package visualizer.components

import visualizer.models.{SelectionController, WaveFormController}

import scala.swing.{BorderPanel, Dimension, ScrollPane, TextArea}
import BorderPanel.Position._

class DependencyComponent(dataModel: SelectionController, displayModel: WaveFormController) extends BorderPanel {
  preferredSize = new Dimension(1000, 100)
  val textComponent: TextArea = new TextArea {
    editable = false
  }
  val scrollPane = new ScrollPane(textComponent)
  add(scrollPane, Center)
}