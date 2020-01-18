package visualizer.models

import firrtl.ir.SIntType
import treadle.executable.{SignedInt, Symbol}
import treadle.{extremaOfSIntOfWidth, extremaOfUIntOfWidth}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

///////////////////////////////////////////////////////////////////////////
// Transition, Waveform, Signal
///////////////////////////////////////////////////////////////////////////
case class Transition[T](timestamp: Long, value: T)

// type Waveform[A] = ArrayBuffer[Transition[A]]
class Waveform[T](val transitions: ArrayBuffer[Transition[T]]) {

  // Return iterator starting from the transition at the timestamp or the
  // transition before the timestamp. If timestamp is before the first transition,
  // return the first transition
  def findTransition(timestamp: Long): Iterator[Transition[T]] = {
    @scala.annotation.tailrec
    def search(low: Int = 0, high: Int = transitions.size - 1): ArrayBuffer[Transition[T]] = {
      val mid = (low + high) / 2

      if (low > high) {
        if (low == 0) transitions else transitions.drop(low - 1)
      } else if (transitions(mid).timestamp == timestamp) {
        transitions.drop(mid)
      } else if (transitions(mid).timestamp > timestamp) {
        search(low, mid - 1)
      } else {
        search(mid + 1, high)
      }
    }
    search().iterator
  }

  def addNewValues(newValues: ArrayBuffer[Transition[T]]): Unit = {
    transitions.clear()
    transitions ++= newValues

    // TODO: remove the line below once isBin is fixed
    isBin match {
      case Some(true) =>
        if (newValues.init.exists(t => t.value != 0 && t.value != 1)) {
          isBin = Some(false)
        }
      case _ =>
    }
  }

  // TODO: should be replaced with information from VCD or treadle about num of bits
  private var isBin: Option[Boolean] = None
  def isBinary: Boolean = {
    isBin match {
      case Some(i) => i
      case None =>
        val res = !transitions.init.exists(t => t.value != 0 && t.value != 1)
        isBin = Some(res)
        res
    }
  }
}

abstract class Signal[T] {
  var waveform: Option[Waveform[T]]

  def addNewValues(newValues: ArrayBuffer[Transition[T]]): Unit = {
    waveform match {
      case Some(w) =>
        w.addNewValues(newValues)
      case None =>
        waveform = Some(new Waveform(newValues))
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Types of signals
///////////////////////////////////////////////////////////////////////////

/** A basic signal representing one wire from a firrtl file or a vcd file
  *
  * @param name      fully qualified name of signal
  * @param symbolOpt pointer to a treadle symbol if there is one
  * @param waveform  data to plot
  * @param sortGroup ordering of this against others, currently not supported
  * @param width     bitwidth of signal, could come from treadle or vcd
  */
class PureSignal(var name:      String,
                 var symbolOpt: Option[Symbol],
                 var waveform:  Option[Waveform[BigInt]],
                 val sortGroup: Int,
                 width:         Int = 1)
    extends Signal[BigInt] {
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
}

/** Aggregates the read and valid signals and the wires under bits
  *
  * @param name       Name of this group
  * @param symbolOpt  //TODO: Does this need to be present
  * @param waveform   //TODO: Do I need this?
  * @param sortGroup  //TODO: Unused, should it be resuscitated
  */
class DecoupledSignalGroup(
  var name:        String,
  var symbolOpt:   Option[Symbol],
  var waveform:    Option[Waveform[BigInt]],
  val sortGroup:   Int, // (IOs, registers, other, Ts and Gens)
  val readySignal: PureSignal,
  val validSignal: PureSignal,
  val bitsSignals: Seq[PureSignal]
) extends Signal[BigInt] {

  def updateValues(): Unit = {
    val combinedTransitions =
      DecoupledSignalGroup.combineReadyValid(readySignal.waveform.get.transitions, validSignal.waveform.get.transitions)

    waveform match {
      case Some(w) =>
        w.addNewValues(combinedTransitions)
      case None =>
        waveform = Some(new Waveform(combinedTransitions))
    }
  }
}

object DecoupledSignalGroup {
  val Fired: BigInt = BigInt(3)
  val Ready: BigInt = BigInt(2)
  val Valid: BigInt = BigInt(1)
  val Busy:  BigInt = BigInt(0)

  def combineReadyValid(a: Seq[Transition[BigInt]], b: Seq[Transition[BigInt]]) = {
    def combinedValue(value1: BigInt, value2: BigInt): BigInt = {
      (value1 > 0, value2 > 0) match {
        case (true, true)   => Fired
        case (true, false)  => Ready
        case (false, true)  => Valid
        case (false, false) => Busy
      }
    }

    val transitions = new mutable.ArrayBuffer[Transition[BigInt]]

    var index1 = 0
    var index2 = 0
    var currentT1: Transition[BigInt] = a.headOption.getOrElse(Transition(0L, 0))
    var currentT2: Transition[BigInt] = b.headOption.getOrElse(Transition(0L, 0))
    var lastT1:    Transition[BigInt] = currentT1
    var lastT2:    Transition[BigInt] = currentT2

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
    transitions
  }
}

class CombinedSignal(
  val pureSignals: Array[PureSignal],
  var waveform:    Option[Waveform[Array[BigInt]]]
) extends Signal[Array[BigInt]]

///////////////////////////////////////////////////////////////////////////
// Ready Valid Combiner
///////////////////////////////////////////////////////////////////////////
object ReadyValidCombiner {
  def apply(sourceSignals: Array[PureSignal]): CombinedSignal = {
    require(sourceSignals.length == 2)
    assert(sourceSignals.forall(pureSignal => pureSignal.waveform.nonEmpty))

    val readyIterator = sourceSignals.head.waveform.get.transitions.iterator
    val validIterator = sourceSignals.last.waveform.get.transitions.iterator
    var readyCurrent = readyIterator.next()
    var validCurrent = validIterator.next()
    var prevReadyVal: BigInt = -1
    var prevValidVal: BigInt = -1
    val readyValidTransitions = new ArrayBuffer[Transition[Array[BigInt]]]()

    assert(readyCurrent.timestamp == validCurrent.timestamp)

    while (readyIterator.hasNext && validIterator.hasNext) {
      if (readyCurrent.timestamp < validCurrent.timestamp) {
        prevReadyVal = readyCurrent.value
        readyValidTransitions += Transition[Array[BigInt]](readyCurrent.timestamp, Array(prevReadyVal, prevValidVal))
        readyCurrent = readyIterator.next()
      } else if (readyCurrent.timestamp > validCurrent.timestamp) {
        prevValidVal = validCurrent.value
        readyValidTransitions += Transition[Array[BigInt]](validCurrent.timestamp, Array(prevReadyVal, prevValidVal))
        validCurrent = validIterator.next()
      } else {
        prevReadyVal = readyCurrent.value
        prevValidVal = validCurrent.value
        readyValidTransitions += Transition[Array[BigInt]](readyCurrent.timestamp, Array(prevReadyVal, prevValidVal))
        readyCurrent = readyIterator.next()
        validCurrent = validIterator.next()
      }
    }

    readyValidTransitions += Transition[Array[BigInt]](math.min(readyCurrent.timestamp, validCurrent.timestamp), null)
    new CombinedSignal(sourceSignals, Some(new Waveform(readyValidTransitions)))
  }
}
