package visualizer

import scala.swing.Component
import scala.swing.event.{ActionEvent, Event}
import treadle.executable.Symbol

//
// Events
//
case class SignalsChanged(override val source: Component) extends ActionEvent(source)
case class WaveFormatChanged(override val source: Component) extends ActionEvent(source)
case class ScaleChanged(override val source: Component) extends ActionEvent(source)
case class CursorSet(override val source: Component) extends ActionEvent(source)
case class MarkerChanged(timestamp: Long, override val source: Component) extends ActionEvent(source)
case class TimeUnitsChanged(override val source: Component) extends ActionEvent(source)

case class DependencyComponentRequested(
  symbols: Seq[Symbol], override val source: Component
) extends ActionEvent(source)

case class SourceInfoRequested(symbols: Seq[Symbol], override val source: Component) extends ActionEvent(source)

class MaxTimestampChanged extends Event
class PureSignalsChanged extends Event