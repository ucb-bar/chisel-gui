// See README.md for license details.

package visualizer.models

import treadle.TreadleTester
import treadle.executable.Symbol

import scala.collection.mutable

object DecoupledHandler {
  val ReadyName = "_ready"
  val ValidName = "_valid"
  val BitsName = "_bits_"

  case class Updater(pattern: String, f: (DecoupledHandler, Symbol) => Unit)

  val updaters = Seq(
    Updater(ReadyName, (d, s) => d.readySymbolOpt = Some(s)),
    Updater(ValidName, (d, s) => d.validSymbolOpt = Some(s)),
    Updater(BitsName, (d, s) => d.bits += s)
  )

  var _indexId: Long = -1L

  def assignIndex(): Long = {
    _indexId += 1L
    _indexId
  }

  val signalNameToDecouple: mutable.HashMap[String, DecoupledHandler] = new mutable.HashMap()

  def prefix(s: String, index: Int): String = {
    s.take(index)
  }

  def lookForReadyValidBundles(tester: TreadleTester): Unit = {
    val engine = tester.engine
    val symbolTable = engine.symbolTable
    symbolTable.nameToSymbol.keys.toSeq.sorted.foreach { symbolName =>
      for (updater <- updaters) {
        val index = symbolName.indexOf(updater.pattern)
        if (index > 0) {
          val prefix = symbolName.take(index)

          val decoupledHandler = signalNameToDecouple.getOrElseUpdate(prefix, apply(prefix))
          updater.f(
            decoupledHandler,
            symbolTable(symbolName)
          )
        }
      }
    }
  }

  def apply(prefix: String): DecoupledHandler = {
    DecoupledHandler(assignIndex(), prefix)
  }
}

case class DecoupledHandler(indexId: Long, prefix: String) {
  var readySymbolOpt: Option[Symbol] = None
  var validSymbolOpt: Option[Symbol] = None
  val bits: mutable.ArrayBuffer[Symbol] = new mutable.ArrayBuffer()
}
