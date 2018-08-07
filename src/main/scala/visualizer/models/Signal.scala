package visualizer.models

import visualizer.Util

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
    def search(low: Int = 0, high: Int = transitions.size - 1): ArrayBuffer[Transition[T]] = {
      val mid = (low + high)/2

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
    assert(newValues.length >= 2)
    assert(transitions.length >= 2)
    assert(transitions.last.timestamp == newValues.head.timestamp,
      s"${transitions.length} ${newValues.length} \n ${Util.transitionsToString(transitions)} \n ${Util.transitionsToString(newValues)}")

    transitions -= transitions.last
    if (transitions.last.value == newValues.head.value) {
      transitions ++= newValues.tail
    } else {
      transitions ++= newValues
    }

    // TODO: remove the linew below once isBin is fixed
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
      case Some(w) => w.addNewValues(newValues)
      case None => waveform = Some(new Waveform(newValues))
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Three types of signals
///////////////////////////////////////////////////////////////////////////
class PureSignal(
  var name: String,
  var waveform: Option[Waveform[BigInt]],
  val sortGroup: Int // (IOs, registers, other, Ts and Gens)
) extends Signal[BigInt]

class TruncatedSignal(
  val pureSignal: PureSignal,
  val bits: ArrayBuffer[Int],
  var waveform: Option[Waveform[BigInt]]
) extends Signal[BigInt]

class CombinedSignal(
  val pureSignals: Array[PureSignal],
  var waveform: Option[Waveform[Array[BigInt]]]
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
