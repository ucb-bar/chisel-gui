// See README.md for license details.

package visualizer.models

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable

object DecoupledFireRestrictor {

  case class Interval(start: Long, end: Long, value: BigInt)

  def buildTimeVector(buffer: mutable.ArrayBuffer[Transition[BigInt]]): List[Interval] = {
    val b = buffer.toList.sliding(2)
    val newList = b.flatMap {
      case transition1 :: transition2 :: Nil =>
        List(
          Interval(transition1.timestamp, transition2.timestamp, transition1.value),
          Interval(transition1.timestamp, transition2.timestamp, transition1.value)
        )
      case transition :: Nil =>
        List.empty
    }
    newList
    }.toList
}

class DecoupledFireRestrictorTest extends FreeSpec with Matchers {
  "select from an array based on values" in {}
}
