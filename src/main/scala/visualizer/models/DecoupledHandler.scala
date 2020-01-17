// See README.md for license details.

package visualizer.models

import treadle.TreadleTester
import treadle.executable.Symbol

import scala.collection.mutable

/** This builds a global list of groups of signal names that seem to be part
  * of a DecoupledBundle
  *
  */
object DecoupledHandler {
  val ReadyName = "_ready"
  val ValidName = "_valid"
  val BitsName = "_bits_"

  val decoupledNames = new mutable.HashSet[String]

  case class Updater(pattern: String, f: (DecoupledHandler, String) => Unit)

  val updaters = Seq(
    Updater(ReadyName, (d, s) => d.readyNameOpt = Some(s)),
    Updater(ValidName, (d, s) => d.validNameOpt = Some(s)),
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

  def lookForReadyValidBundles(names: Seq[String]): Unit = {

    names.sorted.foreach { symbolName =>
      for (updater <- updaters) {
        val index = symbolName.indexOf(updater.pattern)
        if (index > 0) {
          val prefix = symbolName.take(index)

          val decoupledHandler = signalNameToDecouple.getOrElseUpdate(prefix, apply(prefix))
          updater.f(decoupledHandler, symbolName)
          decoupledNames += symbolName
        }
      }
    }
  }

  def apply(prefix: String): DecoupledHandler = {
    DecoupledHandler(assignIndex(), prefix)
  }
}

case class DecoupledHandler(indexId: Long, prefix: String) {
  var readyNameOpt: Option[String] = None
  var validNameOpt: Option[String] = None
  val bits:         mutable.ArrayBuffer[String] = new mutable.ArrayBuffer()

  def fullName: String = s"${prefix}Decoupled"
}
