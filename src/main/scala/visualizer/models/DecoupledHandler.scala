// See README.md for license details.

package visualizer.models

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

  case class Updater(pattern: String, add: (DecoupledHandler, String) => Unit, isMatch: String => Int)

  def hasPattern(pattern: String)(s: String): Int = s.indexOf(pattern)

  def hasPatternAtEnd(pattern: String)(s: String): Int = if (s.endsWith(pattern)) s.indexOf(pattern) else -1

  val updaters = Seq(
    Updater(ReadyName, (d, s) => d.readyNameOpt = Some(s), hasPatternAtEnd(ReadyName)),
    Updater(ValidName, (d, s) => d.validNameOpt = Some(s), hasPatternAtEnd(ValidName)),
    Updater(BitsName, (d, s) => d.bits += s, hasPattern(BitsName))
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
        val index = updater.isMatch(symbolName)
        if (index > 0) {
          val prefix = symbolName.take(index)

          val decoupledHandler = signalNameToDecouple.getOrElseUpdate(prefix, apply(prefix))
          updater.add(decoupledHandler, symbolName)
          decoupledNames += symbolName
        }
      }
    }

    signalNameToDecouple.retain { case (key, d) => d.readyNameOpt.isDefined && d.validNameOpt.isDefined }
  }

  def apply(prefix: String): DecoupledHandler = {
    DecoupledHandler(assignIndex(), prefix)
  }
}

case class DecoupledHandler(indexId: Long, prefix: String) {
  var readyNameOpt: Option[String] = None
  var validNameOpt: Option[String] = None
  val bits: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer()

  def fullName: String = s"${prefix}Decoupled"

  def getChildNames: Seq[String] = {
    bits ++ readyNameOpt ++ validNameOpt
  }
}
