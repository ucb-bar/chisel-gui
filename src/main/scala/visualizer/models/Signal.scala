package visualizer.models

import treadle.executable.{SignedInt, Symbol}
import treadle.{extremaOfSIntOfWidth, extremaOfUIntOfWidth}

import scala.collection.mutable

///////////////////////////////////////////////////////////////////////////
// Transition, Waveform, Signal
///////////////////////////////////////////////////////////////////////////
case class Transition(timestamp: Long, value: BigInt)

abstract class Signal {
  def name: String
}

///////////////////////////////////////////////////////////////////////////
// Types of signals
///////////////////////////////////////////////////////////////////////////

/** A basic signal representing one wire from a firrtl file or a vcd file
  *
  * @param name      fully qualified name of signal
  * @param symbolOpt pointer to a treadle symbol if there is one
  * @param width     bitwidth of signal, could come from treadle or vcd
  */
case class PureSignal(name: String, symbolOpt: Option[Symbol], width: Int = 1) extends Signal {
  //
  // This block is to help with showing signal as xy plot
  //
  val isSInt: Boolean = symbolOpt.isDefined && symbolOpt.get.dataType == SignedInt
  val (minValue: BigInt, maxValue: BigInt) = if (isSInt) {
    extremaOfSIntOfWidth(width)
  } else {
    extremaOfUIntOfWidth(width)
  }
  val range: BigInt = maxValue - minValue

  def scaledValue(x: BigInt): BigDecimal = {
    BigDecimal(x - minValue) / BigDecimal(range)
  }

  def isBinary: Boolean = width == 1
}

/** Aggregates the read and valid signals and the wires under bits
  *
  * @param name Name of this group
  */
class DecoupledSignalGroup(
                            var name: String,
                            val readySignal: PureSignal,
                            val validSignal: PureSignal,
                            val bitsSignals: Seq[PureSignal]
                          ) extends Signal {

  def updateValues(): Unit = {
    DecoupledSignalGroup.combineReadyValid(name, readySignal.name, validSignal.name)
  }
}

object DecoupledSignalGroup {
  val Fired: BigInt = BigInt(3)
  val Ready: BigInt = BigInt(2)
  val Valid: BigInt = BigInt(1)
  val Busy:  BigInt = BigInt(0)

  def combineReadyValid(combinedName: String, readyName: String, validName: String): Unit = {
    def combinedValue(value1: BigInt, value2: BigInt): BigInt = {
      (value1 > 0, value2 > 0) match {
        case (true, true) => Fired
        case (true, false) => Ready
        case (false, true) => Valid
        case (false, false) => Busy
      }
    }

    val (a, b) = (Waves(readyName).toTransitions, Waves(validName).toTransitions)
    val combinedWave = Waves(combinedName)

    var index1 = 0
    var index2 = 0

    var currentT1: Transition = a.headOption.getOrElse(Transition(0L, 0))
    var currentT2: Transition = b.headOption.getOrElse(Transition(0L, 0))
    var lastT1: Transition = currentT1
    var lastT2: Transition = currentT2

    val transitions = new mutable.ArrayBuffer[Transition]()

    while (index1 < a.length || index2 < b.length) {
      if (currentT1.timestamp == currentT2.timestamp) {
        transitions += Transition(currentT1.timestamp, combinedValue(currentT1.value, currentT2.value))
        index1 += 1
        lastT1 = currentT1
        currentT1 = if (index1 < a.length) a(index1) else Transition(Long.MaxValue, currentT1.value)
        index2 += 1
        lastT2 = currentT2
        currentT2 = if (index2 < b.length) b(index2) else Transition(Long.MaxValue, currentT2.value)
      } else if (currentT1.timestamp < currentT2.timestamp) {
        transitions += Transition(currentT1.timestamp, combinedValue(currentT1.value, lastT2.value))
        index1 += 1
        lastT1 = currentT1
        currentT1 = if (index1 < a.length) a(index1) else Transition(Long.MaxValue, currentT1.value)
      } else {
        transitions += Transition(currentT2.timestamp, combinedValue(lastT1.value, currentT2.value))
        index2 += 1
        lastT2 = currentT2
        currentT2 = if (index2 < b.length) b(index2) else Transition(Long.MaxValue, currentT2.value)
      }
    }

    combinedWave.addChanges(transitions)
  }
}

/** Aggregates the signals moderated by a valid signal,
  * We just use the validSignal directly as the state information
  *
  * @param name        Name of this group
  * @param validSignal Valid signal this is based on
  * @param bitsSignals Other signals mediated by this valid
  */
class ValidSignalGroup(
                        var name: String,
                        val validSignal: PureSignal,
                        val bitsSignals: Seq[PureSignal]
                      ) extends Signal {

  def updateValues(): Unit = {
    val validWave = Waves(validSignal.name)
    Waves.nameToWave(name) = validWave
  }
}
