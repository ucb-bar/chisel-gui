package visualizer.models

import scala.collection.mutable

///////////////////////////////////////////////////////////////////////////
// Transition, Waveform, Signal
///////////////////////////////////////////////////////////////////////////
case class Transition[T](timestamp: Long, value: T)

// type Waveform[A] = ArrayBuffer[Transition[A]]
class Waveform[T](val transitions: mutable.ArrayBuffer[Transition[T]]) {

  // Return iterator starting from the transition at the timestamp or the
  // transition before the timestamp. If timestamp is before the first transition,
  // return the first transition
  def findTransition(timestamp: Long): Iterator[Transition[T]] = {
    def search(low: Int = 0, high: Int = transitions.size - 1): mutable.ArrayBuffer[Transition[T]] = {
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

  def addNewValues(newValues: mutable.ArrayBuffer[Transition[T]]): Unit = {
    assert(newValues.length >= 2)
    //assert(transitions.length >= 2)
//    assert(transitions.last.timestamp == newValues.head.timestamp,
//      s"${transitions.length} ${newValues.length} \n ${Util.transitionsToString(transitions)} \n ${Util.transitionsToString(newValues)}")

//    val currentIndex = transitions.indexOf(t => t.timestamp == )
//    transitions -= transitions.last
//    if (transitions.last.value == newValues.head.value) {
//      transitions ++= newValues.tail
//    } else {
//      transitions ++= newValues
//    }

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

/**
  * contains factories for common Waveform types
  */
object Waveform {
  def ofBigInt: Waveform[BigInt] = new Waveform(new mutable.ArrayBuffer[Transition[BigInt]]())
  def ofCombinedBigInt: Waveform[Array[BigInt]] = {
    new Waveform(new mutable.ArrayBuffer[Transition[Array[BigInt]]]())
  }
  def apply[T](transitions: mutable.ArrayBuffer[Transition[T]]): Waveform[T] = {
    new Waveform(transitions)
  }
}

abstract class Signal[T] {
  var waveform: Option[Waveform[T]]

  def addNewValues(newValues: mutable.ArrayBuffer[Transition[T]]): Unit = {
    waveform match {
      case Some(w) => w.addNewValues(newValues)
      case None => waveform = Some(new Waveform(newValues))
    }
  }
}

class CombinedSignal(
  val sourceWaves: Array[WaveNode],
  var waveform: Waveform[ReadyValidState]
)

trait ReadyValidState

object ReadValidStates {
  case object Fired    extends ReadyValidState
  case object ReadySet extends ReadyValidState
  case object ValidSet extends ReadyValidState
  case object Open     extends ReadyValidState

  def getState(readyValue: BigInt, validValue: BigInt): ReadyValidState = {
    (readyValue != BigInt(0), validValue != BigInt(0)) match {
      case (true, true)   => Fired
      case (true, false)  => ReadySet
      case (false, true)  => ValidSet
      case (false, false) => Open
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Ready Valid Combiner
///////////////////////////////////////////////////////////////////////////
object ReadyValidCombiner {
  def apply(sourceSignals: Array[WaveNode]): CombinedSignal = {
    require(sourceSignals.length == 2)
    assert(sourceSignals.forall { _.isInstanceOf[WaveSignal] })

    val readyIterator = sourceSignals.asInstanceOf[WaveSignal].waveform.transitions.iterator
    val validIterator = sourceSignals.asInstanceOf[WaveSignal].waveform.transitions.iterator
    var readyCurrent = readyIterator.next()
    var validCurrent = validIterator.next()
    var prevReadyVal: BigInt = -1
    var prevValidVal: BigInt = -1
    val readyValidTransitions = new mutable.ArrayBuffer[Transition[ReadyValidState]]()

    assert(readyCurrent.timestamp == validCurrent.timestamp)

    while (readyIterator.hasNext && validIterator.hasNext) {
      val timeStamp = if (readyCurrent.timestamp < validCurrent.timestamp) {
        prevReadyVal = readyCurrent.value
        readyCurrent = readyIterator.next()
        readyCurrent.timestamp
      } else if (readyCurrent.timestamp > validCurrent.timestamp) {
        prevValidVal = validCurrent.value
        validCurrent = validIterator.next()
        validCurrent.timestamp
      } else {
        prevReadyVal = readyCurrent.value
        prevValidVal = validCurrent.value
        readyCurrent = readyIterator.next()
        validCurrent = validIterator.next()
        readyCurrent.timestamp
      }

      readyValidTransitions += Transition(timeStamp, ReadValidStates.getState(prevReadyVal, prevValidVal))
    }

    readyValidTransitions += Transition[ReadyValidState](math.min(readyCurrent.timestamp, validCurrent.timestamp), null)
    new CombinedSignal(sourceSignals, Waveform(readyValidTransitions))
  }
}
