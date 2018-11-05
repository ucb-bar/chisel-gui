package visualizer.components

import visualizer.controllers.SelectionController
import visualizer.controllers.WaveFormController

import scala.swing.{BorderPanel, Dimension, ScrollPane, TextArea}
import BorderPanel.Position._

class DependencyComponent(dataModel: SelectionController) extends BorderPanel {
  preferredSize = new Dimension(1000, 100)
  val textComponent: TextArea = new TextArea {
    editable = false
  }
  val scrollPane = new ScrollPane(textComponent)
  add(scrollPane, Center)
}