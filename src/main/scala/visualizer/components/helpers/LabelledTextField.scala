// See README.md for license details.

package visualizer.components.helpers

import scala.swing.{BoxPanel, Dimension, Label, Orientation, TextField}

class LabelledTextField(val labelText: String, initialData: String)(thunk: () => Unit)
    extends BoxPanel(Orientation.Horizontal) {
  val label: Label = new Label(labelText)
  val textField: TextField = new TextField(initialData) {
    maximumSize = new Dimension(150, 15)
  }
  contents += label
  contents += textField

  def setText(newText: String): Unit = {
    textField.text = newText
  }

  def action(): Unit = thunk()
}
