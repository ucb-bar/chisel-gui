package visualizer.components

import visualizer.models.{DataModel, DisplayModel}

import scala.swing.{BorderPanel, Dimension, ScrollPane, TextArea}
import BorderPanel.Position._

class DependencyComponent(dataModel: DataModel, displayModel: DisplayModel) extends BorderPanel {
  preferredSize = new Dimension(800, 100)
  val textComponent: TextArea = new TextArea {
    editable = false
  }
  val scrollPane = new ScrollPane(textComponent)
  add(scrollPane, Center)
}