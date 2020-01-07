package visualizer

import treadle.TreadleTester

import scala.collection.mutable
import treadle.executable.WaveformValues
import treadle.vcd.VCD
import visualizer.models._

import scala.collection.mutable.ArrayBuffer

object Util {
  def toValueChange(waveformValues: WaveformValues,
                    initializing:   Boolean): mutable.HashMap[String, ArrayBuffer[Transition[BigInt]]] = {
    val hashMap = new mutable.HashMap[String, ArrayBuffer[Transition[BigInt]]]()
    waveformValues.symbols.zip(waveformValues.symbolValues).foreach {
      case (symbol, values) =>
        hashMap += symbol.name -> rollbackValuesToTransitions(waveformValues.clockValues, values, initializing)
    }
    hashMap
  }

  def vcdToTransitions(vcd: VCD, initializing: Boolean): mutable.HashMap[String, ArrayBuffer[Transition[BigInt]]] = {
    val nameToTransitions = new mutable.HashMap[String, ArrayBuffer[Transition[BigInt]]] {
      override def default(key: String): ArrayBuffer[Transition[BigInt]] = {
        this(key) = new ArrayBuffer[Transition[BigInt]]
        this(key)
      }
    }
    vcd.valuesAtTime.keys.toSeq.sorted.foreach { time =>
      vcd.valuesAtTime(time).foreach { change =>
        val name = change.wire.fullName
        if (!name.contains("/")) {
          val transitions = nameToTransitions(name)
          if (transitions.isEmpty && initializing && time > 0) {
            transitions += Transition(0L, BigInt(0))
          }
          transitions += Transition(time, change.value)
        }
      }
    }
    nameToTransitions
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
  def rollbackValuesToTransitions(clkValues:      Array[BigInt],
                                  rollbackValues: Array[BigInt],
                                  initializing:   Boolean): ArrayBuffer[Transition[BigInt]] = {
    val buf = new ArrayBuffer[Transition[BigInt]]()

    if (clkValues.nonEmpty && rollbackValues.nonEmpty) {
      var values:      Array[BigInt] = rollbackValues
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

      values.zip(clockValues).tail.foreach {
        case (value, timestamp) =>
          if (value != previousValue) {
            buf += Transition[BigInt](timestamp.toLong, value)
            previousValue = value
            previousTimestamp = timestamp
          }
      }
    }

    buf
  }

  def transitionsToString[T](transitions: ArrayBuffer[Transition[T]]): String = {
    transitions.map(t => s"(${t.timestamp}, ${t.value})").mkString(" ")
  }

  def sortGroup(fullName: String, testerOpt: Option[TreadleTester]): Int = {
    if (testerOpt.isDefined && testerOpt.get.isRegister(fullName)) {
      1
    } else {
      val signalName = fullName.split("\\.").last
      if (signalName.contains("io_")) {
        0
      } else if (signalName.contains("T_") || signalName.contains("GEN_")) {
        3
      } else {
        2
      }
    }
  }
}
