// See README.md for license details.

package visualizer.models

import treadle.vcd.VCD

import scala.collection.Searching._
import scala.collection.mutable

/** Represents the segments of a wave. Each segment has a start, an end and a value
  * starts and ends must be in strictly increasing order
  * end must be greater than the start at the same index
  * starts(0) must be 0
  *
  */
class Wave {
  val starts: mutable.ArrayBuffer[Long] = new mutable.ArrayBuffer()
  val values: mutable.ArrayBuffer[BigInt] = new mutable.ArrayBuffer()

  /** Use system binary search to find index of start time less than or equal to time
    *
    * @param time find the index in starts for this time
    */
  def findStartIndex(time: Long): Int = {
    assert(time >= 0)
    starts.search(time) match {
      case InsertionPoint(insertionPointIndex) =>
        if (insertionPointIndex > 0) {
          insertionPointIndex - 1
        } else {
          0
        }
      case Found(index) => index
      case _ =>
        throw new Exception(s"Searching for time $time, could not find index")
    }
  }

  def add(time: Long, value: BigInt): Unit = {
    if (starts.isEmpty && time > 0) {
      starts += 0L
      values += BigInt(0)
    }
    starts += time
    values += value
  }

  def start(index: Int): Long = starts(index)

  def end(index: Int): Long = if (index < length - 1) starts(index + 1) else starts(index) + 1

  def value(index: Int): BigInt = values(index)

  def length:   Int = starts.length
  def nonEmpty: Boolean = starts.nonEmpty
  def isEmpty:  Boolean = starts.isEmpty
  def indices:  Range = starts.indices

  def toTransitions: Seq[Transition] = {
    starts.indices.map { index =>
      Transition(starts(index), value(index))
    }
  }

  def clear(): Unit = {
    starts.clear()
    values.clear()
  }

  /** Adds a series of changes to this Wave
    *
    * @param transitions list of transitions for this wave
    */
  def addChanges(transitions: Seq[Transition]): Unit = {
    clear()

    var index = 0
    while (index < transitions.length) {
      add(transitions(index).timestamp, transitions(index).value)
      index += 1
    }
  }

  def addOneTransition(transition: Transition): Unit = {
    add(transition.timestamp, transition.value)
  }

  override def toString: String = {
    indices.map { i => s"(${start(i)},${end(i)},${value(i)})" }.mkString(", ")
  }
}

object Waves {
  var vcd: VCD = new VCD("", "", "", "", "", false)

  val nameToWave: mutable.HashMap[String, Wave] = new mutable.HashMap[String, Wave]()

  def get(name: String): Option[Wave] = nameToWave.get(name)

  def exists(name: String): Boolean = {
    nameToWave.contains(name)
  }

  def apply(name: String): Wave = {
    if (!nameToWave.contains(name)) {
      println(s"Problems, not finding Waves($name)")
    }
    nameToWave(name)
  }

  def setVcd(newVcd: VCD): Unit = {
    nameToWave.clear()
    vcd = newVcd
  }

  def addEntryFor(name: String): Unit = {
    if (!nameToWave.contains(name)) nameToWave(name) = new Wave
  }

  /** This will scan the VCD for all change events associated with name
    * Use this function when a new wave is created,
    * typically when a signal is moved to the signal and wave panel
    *
    * @param name the wave to be updated
    */
  def updateWave(name: String): Unit = {
    addEntryFor(name)
    val wave = nameToWave.getOrElseUpdate(name, new Wave)
    val transitions = new mutable.ArrayBuffer[Transition]
    vcd.valuesAtTime.foreach { case (time, changeSet) =>
      changeSet.foreach { change =>
        if(change.wire.fullName == name) {
          transitions += Transition(time, change.value)
        }
      }
    }
    val sortedTransitions = transitions.sortBy(transition => transition.timestamp)
    wave.addChanges(sortedTransitions)
  }

  /** This will update only the existing waves with all the
    * relevant values in the VCD
    *
    */
  def refreshWaves(altVcd: VCD): Unit = {
    nameToWave.values.foreach(_.clear())
    vcd.events.foreach { time =>
      vcd.valuesAtTime(time).foreach { change =>
        nameToWave.get(change.wire.fullName).foreach { wave =>
          wave.addOneTransition(Transition(time, change.value))
        }
      }
    }
  }
}
