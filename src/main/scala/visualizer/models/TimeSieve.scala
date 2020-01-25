// See README.md for license details.

package visualizer.models

import scala.collection.Searching._
import scala.collection.mutable

case class TimeSegment(start: Long, stop: Long, accumulatedTime: Long)

class TimeSieve {
  val starts:   mutable.ArrayBuffer[Long] = new mutable.ArrayBuffer()
  val ends:     mutable.ArrayBuffer[Long] = new mutable.ArrayBuffer()
  val timeSums: mutable.ArrayBuffer[Long] = new mutable.ArrayBuffer()

  def add(start: Long, end: Long): Unit = {
    if (isEmpty) {
      timeSums += 0L
    } else {
      timeSums += timeSum(timeSums.length - 1) + (end - start)
    }

    starts += start
    ends += end
  }

  def start(index:   Int): Long = starts(index)
  def end(index:     Int): Long = ends(index)
  def timeSum(index: Int): Long = timeSums(index)

  def length:   Int = starts.length
  def nonEmpty: Boolean = starts.nonEmpty
  def isEmpty:  Boolean = starts.isEmpty
  def indices:  Range = starts.indices

  def logicalTimeStart(index: Int): Long = {
    timeSum(index)
  }

  def logicalTimeEnd(index: Int): Long = {
    timeSum(index) + end(index)
  }

  def logicalTimeToSieveTime(time: Long): Long = {
    val index = findTimeSumIndex(time)
    if(index >= 0) {
      val segmentStart = start(index)
      segmentStart + (time - timeSum(index))
    } else {
      ends.last
    }
  }

  def sieveTimeToLogicalTime(time: Long): Long = {
    val index = findStartIndex(time)
    if(index >= 0) {
      val accTime = timeSum(index)
      accTime + (time - start(index))
    } else {
      timeSums.last
    }
  }

  /** This answers the question, if I want to display numbers from unseived time, what is the
    * index of the synthetic time range I should use
    *
    * Use system binary search to find index of timeSums
    * unlike a Wave this can fail, which is indicated by returning -1
    *
    * @param time find the index in starts for this time
    */
  def findTimeSumIndex(time: Long): Int = {
    assert(time >= 0)
    timeSums.search(time) match {
      case InsertionPoint(insertionPointIndex) =>
        if (insertionPointIndex > 0 && time < logicalTimeEnd(insertionPointIndex - 1)) {
          insertionPointIndex - 1
        } else if (insertionPointIndex >= length) {
          -1
        } else {
          insertionPointIndex
        }
      case Found(index) => index
      case _ =>
        -1
    }
  }

  /** This answers the question, if I want to display numbers from unseived time, what is the
    * index of the synthetic time range I should use
    *
    * Use system binary search to find index of starts
    * unlike a Wave this can fail, which is indicated by returning -1
    *
    * @param time find the index in starts for this time
    */
  def findStartIndex(time: Long): Int = {
    assert(time >= 0)
    starts.search(time) match {
      case InsertionPoint(insertionPointIndex) =>
        if (insertionPointIndex > 0 && time < logicalTimeEnd(insertionPointIndex - 1)) {
          insertionPointIndex - 1
        } else if (insertionPointIndex >= length) {
          -1
        } else {
          insertionPointIndex
        }
      case Found(index) => index
      case _ =>
        -1
    }
  }

  /** Takes a wave an returns a new synthetic wave with only the times
    * specified by this sieve
    *
    * @param wave         wave to be strained
    * @param startTime    time to start straining
    * @return
    */
  def strain(wave: Wave, startTime: Long): Wave = {
    var time = startTime
    var sieveIndex = findTimeSumIndex(time)
    val waveLength = wave.length

    val strainedWave = new Wave

    while (sieveIndex >= 0 && sieveIndex < length) {
      val initialStartTime = start(sieveIndex)
      val sieveStart = initialStartTime
      val sieveEnd = end(sieveIndex)

      var waveIndex = wave.findStartIndex(sieveStart)

      var logicalStartTime = logicalTimeStart(sieveIndex)

      //
      // find all the waves that have values within sieveStart and sieveEnd
      //
      var waveStart = if (waveIndex < waveLength) wave.start(waveIndex) else -1
      var waveEnd = if (waveIndex < waveLength) wave.end(waveIndex) else -1

      while (waveIndex < waveLength && waveStart < sieveEnd) {
        // if we get here with have wave segment that is overlaps the current sieve

        strainedWave.add(logicalStartTime, wave.value(waveIndex))
        logicalStartTime += sieveEnd.min(waveEnd) - waveStart.max(sieveStart)

        waveIndex += 1
        waveStart = if (waveIndex < waveLength) wave.start(waveIndex) else -1
        waveEnd = if (waveIndex < waveLength) wave.end(waveIndex) else -1
      }

      time = sieveEnd
      sieveIndex += 1
    }
    strainedWave
  }

  override def toString: String = {
    indices.map { i =>
      s"(${start(i)},${end(i)},${timeSum(i)})"
    }.mkString(", ")
  }
}
