// See README.md for license details.

package visualizer.config

import scala.collection.mutable
import scala.swing.Color

object ColorTable {

  trait GuiColor

  object waveBackground extends GuiColor

  object waveSignal extends GuiColor

  object waveCursor extends GuiColor

  object waveMarker extends GuiColor

  object waveGrid extends GuiColor

  private val componentToColor = new mutable.HashMap[GuiColor, Color]

  def setDefaultWaveColors() {
    componentToColor(waveBackground) = DrawMetrics.defaultWaveBackground
    componentToColor(waveSignal) = DrawMetrics.defautWaveSignalColor
    componentToColor(waveCursor) = DrawMetrics.defaultWaveCursorColor
    componentToColor(waveMarker) = DrawMetrics.defaultWaveMarkerColor
    componentToColor(waveGrid) = DrawMetrics.defaultWavGridColor
  }

  def setAltWaveColors() {
    componentToColor(waveBackground) = DrawMetrics.altWaveBackground
    componentToColor(waveSignal) = DrawMetrics.altWaveSignalColor
    componentToColor(waveCursor) = DrawMetrics.altWaveCursorColor
    componentToColor(waveMarker) = DrawMetrics.altWaveMarkerColor
    componentToColor(waveGrid) = DrawMetrics.altWavGridColor
  }

  setDefaultWaveColors()

  def apply(guiColor: GuiColor): Color = {
    componentToColor(guiColor)
  }
}
