package visualizer.components

import visualizer.models.{DataModel, DisplayModel}

import scala.swing.{BorderPanel, Dimension, ScrollPane, TextArea}
import BorderPanel.Position._

/** This is for describing the relationships between signals.
  * The relationships are driving or driven by one more other signals
  *
  * @param dataModel    Source of data
  * @param displayModel Source of things selected for waveform view
  */
class ShowDependenciesPanel(dataModel: DataModel, displayModel: DisplayModel) extends BorderPanel {
  preferredSize = new Dimension(1000, 100)
  val textComponent: TextArea = new TextArea {
    editable = false
  }
  val scrollPane = new ScrollPane(textComponent)
  add(scrollPane, Center)
}
