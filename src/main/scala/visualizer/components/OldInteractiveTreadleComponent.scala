package visualizer.components

import visualizer.models.{DataModel, DisplayModel}
import visualizer.{PureSignalsChanged, TreadleController}

import scala.swing._

class OldInteractiveTreadleComponent(dataModel: DataModel, displayModel: DisplayModel) extends GridPanel(2, 1) {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  preferredSize = new Dimension(400, 700)
  val textArea: TextArea = new TextArea {
    editable = false
  }
  val inputArea: GridPanel = new GridPanel(2, 1) {
    val pokeRow: FlowPanel = new FlowPanel {
      var signalsComboBox: ComboBox[String] = new ComboBox(dataModel.ioSignals)
      val textField: TextField = new TextField("0", 5) {
        horizontalAlignment = Alignment.Right
      }
      val button = new Button(Action("Poke") {
        try {
          val value = BigInt(textField.text)
          TreadleController.tester match {
            case Some(tester) =>
              tester.poke(signalsComboBox.selection.item, value) // TODO: Verify value is allowed (number, width)
              textArea.append(s"Poke ${signalsComboBox.selection.item} with value $value\n")
            case None =>
              textArea.text = s"Not connected to treadle"
          }
        } catch {
          case _: NumberFormatException => // TODO: Notify that value is invalid
        }
      })
      contents += new Label("Poke ")
      contents += signalsComboBox
      contents += new Label("with value ")
      contents += textField
      contents += button

      listenTo(TreadleController)
      reactions += {
        case _: PureSignalsChanged =>
          signalsComboBox = new ComboBox(dataModel.ioSignals)
          contents(1) = signalsComboBox
      }
    }

    val stepRow: FlowPanel = new FlowPanel {
      val textField: TextField = new TextField {
        text = "1"
        columns = 5
        horizontalAlignment = Alignment.Right
      }
      val button = new Button(Action("Go") {
        try {
          val n = textField.text.toInt
          TreadleController.tester match {
            case Some(t) =>
              t.step(n)
              dataModel.loadMoreWaveformValues()
              textArea.append(s"Step $n cycles\n")
            case None =>
              textArea.text = s"Not connected to treadle"
          }
        } catch {
          case _: NumberFormatException => // TODO: Notify that value is invalid
        }
      })
      contents += new Label("Step ")
      contents += textField
      contents += button
    }

    contents += pokeRow
    contents += stepRow
  }

  contents += new ScrollPane(textArea)
  contents += inputArea
}
