//// See README.md for license details.
//
//package visualizer.models
//
//import org.scalatest.{FreeSpec, Matchers}
//
//import scala.collection.mutable
//
//class TransitionCombinerTest extends FreeSpec with Matchers {
//  val rand = scala.util.Random
//  rand.setSeed(0L)
//
//  def makeBoolTransitions() = {
//    val range = 0 to rand.nextInt(10)
//
//    val monoTimeSeq = range.scan(rand.nextInt(10)) { case (a, b) => a + b + 1 }.map(_.toLong)
//
//    val trans = monoTimeSeq.scanLeft(Transition(0, BigInt(rand.nextInt(1)))) {
//      case (last: Transition, t: Long) =>
//        Transition(t, BigInt(1) - last.value)
//    }
//    trans
//  }
//
//  "combining random transition arrays should work" in {
//    val m1 = makeBoolTransitions()
//    val m2 = makeBoolTransitions()
//    println(m1)
//    println(m2)
//
//    val m3 = DecoupledSignalGroup.combineReadyValid(m1, m2)
//
//    val times = m3.map(_.timestamp)
//
//    // times should increase monotonically
//    times.zip(times.tail).forall { case (a, b) => a < b } should be (true)
//    // last time of combined should be the max time of m1
//    times.max should be ((m1 ++ m2).map(_.timestamp).max)
//    println(m3)
//  }
//}
