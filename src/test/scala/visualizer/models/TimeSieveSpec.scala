// See README.md for license details.

package visualizer.models

import org.scalatest.{FreeSpec, Matchers}

class TimeSieveSpec extends FreeSpec with Matchers {
  "check search behavior" in {
    val sieve = new TimeSieve

    for (i <- 0 until 3) {
      sieve.add(i * 4, i * 4 + 2)
    }

    println(s"sieve: $sieve")

    sieve.findTimeSumIndex(0) should be(0)
    sieve.findTimeSumIndex(1) should be(0)

    sieve.findTimeSumIndex(2) should be(1)
    sieve.findTimeSumIndex(3) should be(1)

    sieve.findTimeSumIndex(4) should be(2)
    sieve.findTimeSumIndex(5) should be(2)

    sieve.findTimeSumIndex(6) should be(-1)
    sieve.findTimeSumIndex(7) should be(-1)

  }
  "time sieves should reduce waves" in {
    val wave = new Wave

    wave.add(0, 100)
    wave.add(10, 50)
    wave.add(20, 600)
    wave.add(30, 333)
    wave.add(40, 444)
    wave.add(50, 555)

    println(s"wave: $wave")

    val sieve = new TimeSieve

    var wave1 = new Wave

    sieve.add(0, 15)
    println(s"sieve: $sieve")
    wave1 = sieve.strain(wave, 0L)

    wave1.toString should be("(0,10,100), (10,11,50)")
    println(s"wave1: $wave1")

    sieve.add(35, 50)
    println(s"sieve: $sieve")
    wave1 = sieve.strain(wave, 0L)

    wave1.toString should be("(0,10,100), (10,15,50), (15,20,333), (20,21,444)")
    println(s"wave1: $wave1")

  }
}
