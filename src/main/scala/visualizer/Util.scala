package visualizer

import scala.collection.mutable
import treadle.executable.WaveformValues
import visualizer.models._

import scala.collection.mutable.ArrayBuffer

object Util {
  def toValueChange(waveformValues: WaveformValues): mutable.HashMap[String, Waveform[BigInt]] = {
    val hashMap = new mutable.HashMap[String, Waveform[BigInt]]()
    waveformValues.symbols.zip(waveformValues.symbolValues).foreach {
      case (symbol, values) =>
        hashMap += symbol.name -> allValuesToTransitionVec(waveformValues.clockValues, values)
    }
    hashMap
  }

  def allValuesToTransitionVec(clkValues: Array[BigInt], vals: Array[BigInt]): ArrayBuffer[Transition[BigInt]] = {
    val buf = new ArrayBuffer[Transition[BigInt]]()

    var values: Array[BigInt] = vals
    var clockValues: Array[BigInt] = clkValues

    if (clockValues(0) != 0) {
      clockValues +:= BigInt(0)
      values +:= BigInt(0)
    }

    if (values(values.length - 1) != null) {
      clockValues :+= clockValues(clockValues.length - 1) + 10
      values :+= null
    }

    var previousValue = values(0)
    var previousTimestamp = clockValues(0)
    buf += Transition[BigInt](clockValues(0).toLong, values(0))

    values.zip(clockValues).tail.foreach { case (value, timestamp) =>
      if (value != previousValue) {
        buf += Transition[BigInt](timestamp.toLong, value)
        previousValue = value
        previousTimestamp = timestamp
      }
    }
    buf
  }
}
