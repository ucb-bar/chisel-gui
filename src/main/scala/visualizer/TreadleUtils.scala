// See LICENSE for license details.

package visualizer

import treadle.TreadleTester
import treadle.executable.{Symbol, SymbolTable}

import scala.collection.mutable

object TreadleUtils {
  val tester: TreadleTester = TreadleController.tester.get

  /**
    * returns a distinct list of signals either driven by or drivingthe specified
    * symbol, list is approximate depth order
    * list does not contain duplicates
    * @param symbol    symbol to start at
    * @param maxDepth  how deep should the dependencies be followed
    * @param drivenBy  driving or driven by
    * @return
    */
  def getSignalsRelatedTo(symbol: Symbol, maxDepth: Int = 1, drivenBy: Boolean): Seq[Symbol] = {
    val symbolTable = tester.engine.symbolTable
    val diGraph = if(drivenBy) symbolTable.childrenOf else symbolTable.parentsOf
    val symbolsAtDepth = Array.fill(maxDepth + 1) {
      new mutable.HashSet[Symbol]
    }

    def walkGraph(symbol: Symbol, depth: Int): Unit = {
      if(depth > 0) symbolsAtDepth(depth) += symbol

      if (depth < maxDepth) {
        diGraph.getEdges(symbol).toSeq.sortBy(_.name).foreach { childSymbol =>
          walkGraph(childSymbol, depth + 1)
        }
        if (symbolTable.isRegister(symbol.name)) {
          walkGraph(symbolTable(SymbolTable.makeRegisterInputName(symbol)), depth + 1)
        }
      }
    }

    walkGraph(symbol, depth = 0)

    symbolsAtDepth.indices.flatMap { index =>
      symbolsAtDepth(index)
    }.distinct
  }
}
