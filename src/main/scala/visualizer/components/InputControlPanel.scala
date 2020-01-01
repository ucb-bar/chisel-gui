package visualizer.components

import treadle.executable.StopException
import visualizer.{PureSignalsChanged, TreadleController}
import visualizer.models.{DataModel, DisplayModel}

import scala.swing._

class InputControlPanel(dataModel: DataModel, displayModel: DisplayModel) extends GridPanel(2, 1) {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  preferredSize = new Dimension(300, 700)
  val textArea: TextArea = new TextArea {
    editable = false
  }

  listenTo(TreadleController)
  reactions += {
    case _: PureSignalsChanged =>
      fillInputs()
  }

  var bottomPanel: Component = new Label("unloaded")
  contents += new ScrollPane(textArea)
  contents += bottomPanel

  def fillInputs(): Unit = {
    val inputArea: Component = new GridBagPanel {
      def constraints(x:          Int,
                      y:          Int,
                      gridWidth:  Int = 1,
                      gridHeight: Int = 1,
                      weightX:    Double = 0.0,
                      weightY:    Double = 0.0,
                      fill:       GridBagPanel.Fill.Value = GridBagPanel.Fill.None): Constraints = {
        val c = new Constraints
        c.gridx = x
        c.gridy = y
        c.gridwidth = gridWidth
        c.gridheight = gridHeight
        c.weightx = weightX
        c.weighty = weightY
        c.fill = fill
        c
      }

      TreadleController.testerOpt match {
        case Some(tester) =>
          val symbolTable = tester.engine.symbolTable
          val clockNames = tester.clockStepper.clockAssigners.keys.map(_.name).toSet

          val inputNames = symbolTable.inputPortsNames.toSeq.sorted.filterNot(clockNames.contains)
          val inputValues = Seq.fill(inputNames.length) {
            new TextField("0", 5) {
              border = Swing.EtchedBorder(Swing.Lowered)
              horizontalAlignment = Alignment.Right
            }
          }

          var currentRow = 0

          val clockTextField = new TextField {
            text = tester.cycleCount.toString
            columns = 10
            editable = false
          }
          add(new FlowPanel {
            contents += new Label("Clock")
            contents += clockTextField
          }, constraints(0, currentRow))

          currentRow += 1
          for ((inputPort, textField) <- inputNames.zip(inputValues)) {
            add(
              new Label(inputPort) {
                border = Swing.EtchedBorder(Swing.Lowered)
                horizontalAlignment = Alignment.Left
              },
              constraints(0, currentRow, fill = GridBagPanel.Fill.Horizontal)
            )
            add(textField, constraints(1, currentRow))
            currentRow += 1
          }

          def pokeAll(): Unit = {
            for ((inputPort, row) <- inputNames.zipWithIndex) {
              try {
                val textField = inputValues(row)
                val value = BigInt(textField.text)

                tester.poke(inputPort, value) // TODO: Verify value is allowed (number, width)
                textArea.append(s"Poke $inputPort with value $value\n")
              } catch {
                case _: NumberFormatException => // TODO: Notify that value is invalid
              }
            }
          }

          currentRow += 1
          add(Button("Poke") { pokeAll() },
              constraints(0, currentRow, gridWidth = 3, fill = GridBagPanel.Fill.Horizontal))

          currentRow += 1
          add(new Separator(), constraints(0, currentRow, gridWidth = 2))

          currentRow += 1

          add(
            new Label("Number of steps") {
              border = Swing.EtchedBorder(Swing.Lowered)
              horizontalAlignment = Alignment.Left
            },
            constraints(0, currentRow, fill = GridBagPanel.Fill.Horizontal)
          )
          val stepsToTakeInput = new TextField("1", 5) {
            border = Swing.EtchedBorder(Swing.Lowered)
            horizontalAlignment = Alignment.Right
          }
          add(stepsToTakeInput, constraints(1, currentRow))

          currentRow += 1
          add(
            Button("Step") {
              try {
                val value = BigInt(stepsToTakeInput.text).toInt
                tester.step(value)
                dataModel.loadMoreWaveformValues()
                textArea.append(s"Step $value cycles\n")
                clockTextField.text = tester.cycleCount.toString
              } catch {
                case _: NumberFormatException => // TODO: Notify that value is invalid
                case StopException(message) =>
                  //TODO: Figure out what to do here, should stopped condition be cleared so treadle can keep going
                  textArea.append(message + "\n")
              }

            },
            constraints(0, currentRow, gridWidth = 3, fill = GridBagPanel.Fill.Horizontal)
          )

        case None =>
          textArea.text = s"Not connected to treadle"
      }
    }

    contents -= bottomPanel
    bottomPanel = inputArea
    contents += bottomPanel
  }
}
