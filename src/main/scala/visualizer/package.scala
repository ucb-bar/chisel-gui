import scala.collection.mutable.ArrayBuffer

package object visualizer {
  case class Transition[A](timestamp: Long, value: A)
  type Waveform[A] = ArrayBuffer[Transition[A]]
}
