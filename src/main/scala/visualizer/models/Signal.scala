package visualizer.models

import visualizer.{Transition, Util, Waveform}

import scala.collection.mutable.ArrayBuffer

abstract class Signal[T] {
  val waveform: Waveform[T]

  // Return iterator starting from the transition at the timestamp or the
  // transition before the timestamp. If timestamp is before the first transition,
  // return the first transition
  def findTransition(timestamp: Long): Iterator[Transition[T]] = {
    def search(low: Int = 0, high: Int = waveform.size - 1): ArrayBuffer[Transition[T]] = {
      val mid = (low + high)/2

      if (low > high) {
        if (low == 0) waveform else waveform.drop(low - 1)
      } else if (waveform(mid).timestamp == timestamp) {
        waveform.drop(mid)
      } else if (waveform(mid).timestamp > timestamp) {
        search(low, mid - 1)
      } else {
        search(mid + 1, high)
      }
    }
    search().iterator
  }

  def addNewValues(newValues: Waveform[T]): Unit = {
    assert(waveform.length >= 2)
    assert(newValues.length >= 2)
    assert(waveform.last.timestamp == newValues.head.timestamp,
      s"${waveform.length} ${newValues.length} \n ${Util.waveformToString(waveform)} \n ${Util.waveformToString(newValues)}")

    waveform -= waveform.last
    if (waveform.last.value == newValues.head.value) {
      waveform ++= newValues.tail
    } else {
      waveform ++= newValues
    }
  }

  // TODO: should be replaced with information from VCD or treadle about num of bits
  private var isBin: Option[Boolean] = None
  def isBinary: Boolean = {
    isBin match {
      case Some(i) => i
      case None =>
        val res = !waveform.init.exists(t => t.value != 0 && t.value != 1)
        isBin = Some(res)
        res
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Three types of signals
///////////////////////////////////////////////////////////////////////////
class PureSignal(
  var name: String,
  val waveform: Waveform[BigInt],
  val isRegister: Boolean
) extends Signal[BigInt]

class TruncatedSignal(
  val pureSignal: PureSignal,
  val bits: ArrayBuffer[Int],
  val waveform: Waveform[BigInt]
) extends Signal[BigInt]

class CombinedSignal(
  val pureSignals: Array[PureSignal],
  val waveform: Waveform[Array[BigInt]]
) extends Signal[Array[BigInt]]

///////////////////////////////////////////////////////////////////////////
// Ready Valid Combiner
///////////////////////////////////////////////////////////////////////////
object ReadyValidCombiner {
  def apply(sourceSignals: Array[PureSignal]): CombinedSignal = {
    require(sourceSignals.length == 2)
    assert(sourceSignals.forall(pureSignal => pureSignal.waveform.nonEmpty))

    val readyIterator = sourceSignals.head.waveform.iterator
    val validIterator = sourceSignals.last.waveform.iterator
    var readyCurrent = readyIterator.next()
    var validCurrent = validIterator.next()
    var prevReadyVal: BigInt = -1
    var prevValidVal: BigInt = -1
    val readyValidWaveform = new Waveform[Array[BigInt]]()

    assert(readyCurrent.timestamp == validCurrent.timestamp)

    while (readyIterator.hasNext && validIterator.hasNext) {
      if (readyCurrent.timestamp < validCurrent.timestamp) {
        prevReadyVal = readyCurrent.value
        readyValidWaveform += Transition[Array[BigInt]](readyCurrent.timestamp, Array(prevReadyVal, prevValidVal))
        readyCurrent = readyIterator.next()
      } else if (readyCurrent.timestamp > validCurrent.timestamp) {
        prevValidVal = validCurrent.value
        readyValidWaveform += Transition[Array[BigInt]](validCurrent.timestamp, Array(prevReadyVal, prevValidVal))
        validCurrent = validIterator.next()
      } else {
        prevReadyVal = readyCurrent.value
        prevValidVal = validCurrent.value
        readyValidWaveform += Transition[Array[BigInt]](readyCurrent.timestamp, Array(prevReadyVal, prevValidVal))
        readyCurrent = readyIterator.next()
        validCurrent = validIterator.next()
      }
    }

    readyValidWaveform += Transition[Array[BigInt]](math.min(readyCurrent.timestamp, validCurrent.timestamp), null)
    new CombinedSignal(sourceSignals, readyValidWaveform)
  }
}
