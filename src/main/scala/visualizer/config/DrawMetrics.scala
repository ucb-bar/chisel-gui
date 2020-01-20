package visualizer.config

import java.awt.Color

object DrawMetrics {
  val Foo: Int = 5
  val WaveformHeight: Int = 20
  val WaveformVerticalGap: Int = 5
  val WaveformVerticalSpacing: Int = WaveformHeight + 2 * WaveformVerticalGap
  val MinorTicksPerMajor: Int = 10
  val TimescaleHeight: Int = 25
  val MinorTickTop: Int = 18
  val MinMinorTickHSpace: Int = 5
  val MarkerNameXOffset: Int = 5
  val MarkerNameYOffset: Int = -20

  val toggleSelectedBg: Color = Color.darkGray
  val toggleUnselectedBg: Color = Color.lightGray

  val defaultWaveBackground: Color = new Color(240, 240, 240)
  val defautWaveSignalColor: Color = Color.black
  val defaultWaveCursorColor: Color = new Color(39, 223, 85)
  val defaultWaveMarkerColor: Color = Color.black
  val defaultWavGridColor: Color = new Color(200, 200, 255)

  val altWaveBackground: Color = Color.black
  val altWaveSignalColor: Color = Color.green
  val altWaveCursorColor: Color = Color.white
  val altWaveMarkerColor: Color = Color.yellow
  val altWavGridColor: Color = Color.lightGray

  val FireColor: Color = Color.red // new Color(152, 251, 152)
  val ReadySetColor: Color = new Color(255, 240, 106)
  val ValidSetColor: Color = new Color(255, 208, 98)
}
