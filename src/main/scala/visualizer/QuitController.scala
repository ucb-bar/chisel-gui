// See LICENSE for license details.

package visualizer

import java.io.{File, PrintWriter}

import visualizer.controllers.WaveFormController
import visualizer.models._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import scalaswingcontrib.tree.Tree

import scala.io.Source

//class QuitController(waveFormController: WaveFormController) {
class QuitController() {

  def saveWaveformInfo(tree: Tree[SelectionNode], fileName: String) {
    //var jsonResult = null



    // problem, dfs alone does not allow us to store a tree in a way that can be reconstructed
    TreeHelper.viewableDepthFirstIterator(tree).foreach {
      case signal: WaveSignal =>
        // serialize and output

        val temp = ("name" -> signal.name) //~ ("format" -> signal.format)


        // combining the objects
        /*
        if (jsonResult == null) {
          jsonResult = temp
        }
        jsonResult = jsonResult merge temp
        */

        //println(s"Node: ${signal.name}")
        println(s"Node: ${signal.name}")
      case group: WaveGroup =>
        // serialize and output
        //println(s"Group ${signal.}")


      case _ =>
        // generate error
    }






    val json = ("test1" ->
      ("1-1" -> "test1-1") ~
        ("test" -> "testytest"))

    //println(compact(render(json)))

    // converting our data to a string
    // can use "pretty" if we want the disk file to be easier to read
    val res = compact(render(json)) //parse(""" { "numbers" : [1, 2, 3, 4] } """)

    // storing our data in a file
    // TODO: add a standardized location for these files

    val file = new File(fileName)
    val pw = new PrintWriter(file)
    pw.write(res)
    pw.close()

  }


  // tree: an empty tree which we will write our data to following a load
  // fileName: the file on disk to read the tree from
  def readWaveformInfo(fileName: String, tree: Tree[SelectionNode]): Unit = {
    // read the file
    // TODO: add error trapping for file not existing, etc.
    val file = new File(fileName)

    // parse the json
    // note that this could cause problems with really big strings
    val contents = Source.fromFile(fileName).mkString


    //println(contents)
    val parsed = parse(contents)

    println(parsed)



  }
}
