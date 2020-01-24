// See README.md for license details.

package visualizer.models

import org.scalatest.{FreeSpec, Matchers}

class WaveSpec extends FreeSpec with Matchers {
  def makeTransitions(seq: (Long, BigInt)*): Seq[Transition] = {
    seq.map { case (t, v) => Transition(t, v) }
  }

  "Creating a wave gets one automatic entry" in {
    val wave = new Wave
    wave.start(0) should be(0L)
    wave.end(0) should be(Long.MaxValue)
    wave.value(0) should be(BigInt(0))

    wave.findStartIndex(0) should be(0L)
    wave.findStartIndex(1000000L) should be(0L)

    wave.end(0) should be > (0L)
  }

  "Adding segments to wave should work properly" in {
    val wave = new Wave

    var transitions = makeTransitions((0, 50), (10, 40))
    wave.addChanges(transitions)

    wave.length should be(2)
    wave.start(0) should be(0)
    wave.end(0) should be(10)
    wave.value(0) should be(BigInt(50))

    wave.start(1) should be(10)
    wave.end(1) should be(Long.MaxValue)
    wave.value(1) should be(BigInt(40))

    println(s"starts: ${wave.starts}")
    println(s"values: ${wave.values}")

    transitions = makeTransitions((0, 12), (10, 40), (15, 20))
    wave.addChanges(transitions)

    wave.length should be(3)
    wave.start(0) should be(0)
    wave.end(0) should be(10)
    wave.value(0) should be(BigInt(12))

    wave.start(1) should be(10)
    wave.end(1) should be(15)
    wave.value(1) should be(BigInt(40))

    wave.start(2) should be(15)
    wave.end(2) should be(Long.MaxValue)
    wave.value(2) should be(BigInt(20))

    println(s"starts: ${wave.starts}")
    println(s"values: ${wave.values}")

    //
    // Wave can be shortened
    //
    transitions = makeTransitions((0, 7), (2, 4))
    wave.addChanges(transitions)
    wave.length should be(2)

    wave.start(0) should be(0)
    wave.end(0) should be(2)
    wave.value(0) should be(BigInt(7))

    wave.start(1) should be(2)
    wave.end(1) should be(Long.MaxValue)
    wave.value(1) should be(BigInt(4))

    println(s"starts: ${wave.starts}")
    println(s"values: ${wave.values}")
  }

}
