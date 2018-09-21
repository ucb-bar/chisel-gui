package visualizer.components

import visualizer.{PureSignalsChanged, TreadleController}
import visualizer.models.{DataModel, DisplayModel}

import scala.swing._

class InteractiveTreadleComponent(dataModel: DataModel, displayModel: DisplayModel) extends GridPanel(2, 1) {

  ///////////////////////////////////////////////////////////////////////////
  // View
  ///////////////////////////////////////////////////////////////////////////
  preferredSize = new Dimension(400, 700)
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
      def constraints(x: Int, y: Int,
        gridWidth      : Int = 1, gridHeight: Int = 1,
        weightX        : Double = 0.0, weightY: Double = 0.0,
        fill           : GridBagPanel.Fill.Value = GridBagPanel.Fill.None)
      : Constraints = {
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

      TreadleController.tester match {
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

          add(new FlowPanel {
            contents += new Label("Clock")
            contents += new TextField {
              text = "273"
              columns = 10
              editable = false
            }
          }, constraints(0, currentRow))

          currentRow += 1
          for((inputPort, textField) <- inputNames.zip(inputValues)) {
            println(s"Adding port $inputPort to form")

            add(new Label(inputPort), constraints(0, currentRow))
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

          add(Button("Poke") { pokeAll() },
            constraints(0, currentRow, gridWidth=3, fill=GridBagPanel.Fill.Horizontal))

          currentRow += 1
          add(Button("Step") {
            tester.step()
            dataModel.loadMoreWaveformValues()
            textArea.append(s"Step 1 cycles\n")
          },
            constraints(0, currentRow, gridWidth=3, fill=GridBagPanel.Fill.Horizontal))

        case None =>
          textArea.text = s"Not connected to treadle"
      }
    }

    contents -= bottomPanel
    bottomPanel = inputArea
    contents += bottomPanel
  }


  //    val pokeRow: FlowPanel = new FlowPanel {
//      var signalsComboBox: ComboBox[String] = new ComboBox(dataModel.ioSignals)
//      val textField: TextField = new TextField("0", 5) {
//        horizontalAlignment = Alignment.Right
//      }
//      val button = new Button(Action("Poke") {
//        try {
//          val value = BigInt(textField.text)
//          TreadleController.tester match {
//            case Some(tester) =>
//              tester.poke(signalsComboBox.selection.item, value) // TODO: Verify value is allowed (number, width)
//              textArea.append(s"Poke ${signalsComboBox.selection.item} with value $value\n")
//            case None =>
//              textArea.text = s"Not connected to treadle"
//          }
//        } catch {
//          case _: NumberFormatException => // TODO: Notify that value is invalid
//        }
//      })
//      contents += new Label("Poke ")
//      contents += signalsComboBox
//      contents += new Label("with value ")
//      contents += textField
//      contents += button
//
//      listenTo(TreadleController)
//      reactions += {
//        case _: PureSignalsChanged =>
//          signalsComboBox = new ComboBox(dataModel.ioSignals)
//          contents(1) = signalsComboBox
//      }
//    }
//
//    val stepRow: FlowPanel = new FlowPanel {
//      val textField: TextField = new TextField {
//        text = "1"
//        columns = 5
//        horizontalAlignment = Alignment.Right
//      }
//      val button = new Button(Action("Go") {
//        try {
//          val n = textField.text.toInt
//          TreadleController.tester match {
//            case Some(t) =>
//              t.step(n)
//              dataModel.loadMoreWaveformValues()
//              textArea.append(s"Step $n cycles\n")
//            case None =>
//              textArea.text = s"Not connected to treadle"
//          }
//        } catch {
//          case _: NumberFormatException => // TODO: Notify that value is invalid
//        }
//      })
//      contents += new Label("Step ")
//      contents += textField
//      contents += button
//    }
//
//    contents += pokeRow
//    contents += stepRow
//  }

}
