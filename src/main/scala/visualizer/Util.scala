package visualizer

import scala.collection.mutable
import treadle.executable.WaveformValues
import visualizer.models._

import scala.collection.mutable.ArrayBuffer

object Util {
  def toValueChange(waveformValues: WaveformValues, initializing: Boolean): mutable.HashMap[String, Waveform[BigInt]] = {
    val hashMap = new mutable.HashMap[String, Waveform[BigInt]]()
    waveformValues.symbols.zip(waveformValues.symbolValues).foreach {
      case (symbol, values) =>
        hashMap += symbol.name -> rollbackValuesToTransitions(waveformValues.clockValues, values, initializing)
    }
    hashMap
  }

  /**
    * Convert Array of rollback values to an ArrayBuffer of Transitions (value change)
    *
    * If initializing, the first transition will have timestamp 0. The value of the first transition will be 0 unless
    * the rollbackValues starts with clock value 0.
    *
    * @param clkValues timestamps
    * @param rollbackValues values
    * @param initializing true if building a waveform scratch
    * @return
    */
  def rollbackValuesToTransitions(clkValues: Array[BigInt], rollbackValues: Array[BigInt], initializing: Boolean): ArrayBuffer[Transition[BigInt]] = {
    val buf = new ArrayBuffer[Transition[BigInt]]()

    var values: Array[BigInt] = rollbackValues
    var clockValues: Array[BigInt] = clkValues

    if (initializing && clockValues(0) != 0) {
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

  def waveformToString[T](waveform: Waveform[T]): String = {
    waveform.map(t => s"(${t.timestamp}, ${t.value})").mkString(" ")
  }
}
