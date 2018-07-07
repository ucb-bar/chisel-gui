package visualizer

import scala.swing.Component
import scala.swing.event.ActionEvent

//
// Events
//
case class SignalsChanged(override val source: Component) extends ActionEvent(source)
case class ScaleChanged(override val source: Component) extends ActionEvent(source)
case class CursorSet(override val source: Component) extends ActionEvent(source)
case class MarkerChanged(timestamp: Long, override val source: Component) extends ActionEvent(source)
case class TimeUnitsChanged(override val source: Component) extends ActionEvent(source)