// See README.md for license details.

package visualizer.models

import scala.collection.mutable

case class TimeSegment(start: Long, stop: Long)

class TimeSieveModel extends mutable.ArrayBuffer[TimeSegment] {

}
