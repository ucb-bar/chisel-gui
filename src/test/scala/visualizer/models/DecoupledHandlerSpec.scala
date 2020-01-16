// See README.md for license details.

package visualizer.models

import firrtl.FileUtils
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

class DecoupledHandlerSpec extends FreeSpec with Matchers {
  "be able to extract decoupled bundles" in {
    val f = FileUtils.getText("/Users/chick/Adept/dev/research/chisel-gui/samples/DecoupledGcd.fir")

    val t = treadle.TreadleTester(Seq(FirrtlSourceAnnotation(f)))

    DecoupledHandler.lookForReadyValidBundles(t)

    DecoupledHandler.signalNameToDecouple.size should be(2)
  }
}
