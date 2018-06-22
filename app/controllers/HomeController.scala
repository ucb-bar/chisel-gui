package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import org.json4s._
import org.json4s.native.JsonMethods.{render => jsonRender, _}
import treadle._
import treadle.repl._

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */

  val optionsManager = new TreadleOptionsManager with HasReplConfig
  val treadleAPI = new TreadleAPI(optionsManager)
  treadleAPI.executeCommand(Array("load", "../../treadle/samples/gcd.fir"))
  treadleAPI.executeCommand(Array("poke", "io_e", "1"))
  treadleAPI.executeCommand(Array("poke", "io_a", "36"))
  treadleAPI.executeCommand(Array("poke", "io_b", "98"))
  treadleAPI.executeCommand(Array("step"))
  treadleAPI.executeCommand(Array("poke", "io_e", "0"))
  treadleAPI.executeCommand(Array("step", "5"))

  var jsonString : String = """{ "signal":[{ "name":"clk", "wave":"P....." },{ "name":"io_a", "wave":"22....", "data":"0 234 " },{ "name":"io_b", "wave":"22....", "data":"0 38 " },{ "name":"x", "wave":"2.2222", "data":"0 234 196 158 120 " },{ "name":"y", "wave":"2.2...", "data":"0 38 " },{ "name":"io_z", "wave":"2.2222", "data":"0 234 196 158 120 " },{ "name":"io_v", "wave":"222...", "data":"0 1 0 " }], "head":{ "tick":0 } }"""

  treadleAPI.executeCommand(Array("waves", "io_a", "io_b", "x", "y", "io_z", "io_v")) match {
    case Some(json: JValue) => {
      jsonString = pretty(jsonRender(json))
    }
  }

  val y = scala.xml.Unparsed(jsonString)
  val x = <script type="WaveDrom"> {y} </script>

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index(x))
  }
}
