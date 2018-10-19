// See LICENSE for license details.

package visualizer

import org.scalatest.{FreeSpec, Matchers}
import treadle.TreadleTester
import visualizer.models.{SelectionGroup, SignalSelectionModel, TreeHelper}
import scalaswingcontrib.tree.Tree.Path


class SignalSelectionModelSpec extends FreeSpec with Matchers {
  "can create a selection model from a firrtl file" in {
    val stream = getClass.getResourceAsStream("/GCD.fir")
    val input = io.Source.fromInputStream(stream).mkString

    val treadleTester = new TreadleTester(input)

    val model = SignalSelectionModel(treadleTester.engine.symbolTable)

    val path = SignalSelectionModel.RootPath
    val rootChildren = model.getChildrenOf(path).toList
    println(s"root children $rootChildren")

    rootChildren.head.name should be ("TopLevelInputs")
    rootChildren.tail.head.name should be ("TopLevelOutputs")

    val inputs = model.getChildrenOf(path :+ SelectionGroup("TopLevelInputs")).toList

    inputs.head.name should be ("clk")
    inputs.tail.head.name should be ("io_a")

    val parentDir = model.getPathTo("io_z").get
    println(s"parent dir of io_z is ${parentDir.toList}")
    parentDir.map(_.name).mkString(".") should be ("TopLevelOutputs.io_z")

    var count = 0
    model.walk { (path, node) =>
      count += 1
    }

    // count is 2 bigger because of introduced directories for top level IOs
    count should be (treadleTester.engine.symbolTable.size + 2)

    model
  }
}
