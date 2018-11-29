// See LICENSE for license details.

package visualizer

import java.io._
import java.util.Base64

import visualizer.controllers.WaveFormController
import visualizer.models._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scalaswingcontrib.tree.Tree

import scala.io.Source

//class QuitController(waveFormController: WaveFormController) {
class QuitController() {

  def saveWaveformInfo(tree: Tree[SelectionNode], fileName: String) {
    // note that the type of all items returned by the map must be the same
    // (otherwise it will be considered as a List[Any] which seems to not work in Jackson
    val jres = "object" ->
      ("name" -> "WaveForm") ~
      ("items" -> TreeHelper.viewableDepthFirstIterator(tree).toList.map( {
        case signal: WaveSignal =>
          // serialize and output
          ("n" -> signal.name) ~ ("f" -> "add field")
          //("serial" -> serializeToString(signal))
        case group: WaveGroup =>
          // serialize and output
          ("g" -> group.name) ~ ("temp" -> "this is a group")
          //("serial" -> serializeToString(group))
        case _ =>
          // should not be any thing here, throw exception
          ("nothing here" -> "none") ~ ("2" -> "2")
          //("serial" -> "none")
      })
    )

    //println(compact(render(json)))

    // converting our data to a string
    // can use "pretty" if we want the disk file to be easier to read by human
    val res = pretty(render(jres))

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

    try {
      //var file = new File(fileName)

      // parse the json
      // note that this could cause problems with really big strings, such as generated by large circuits
      val contents = Source.fromFile(fileName).mkString

      //println(contents)
      val parsed = parse(contents)


      // TODO reconstruct tree from read objects



      println(parsed)

    } catch {
      case ex: NullPointerException => {println("Null Pointer when loading file \"%s\"".format(fileName))}
      case ex: FileNotFoundException => {println("No saved Waveform file found at \"%s\".".format(fileName))}
    }


  }


  def serializeToString(obj: Serializable): String = {
    // given a serializable object, return a string containing its serialization
    // TODO error handling
    // this is probably inefficient to use for many items

    // via: https://stackoverflow.com/questions/134492/how-to-serialize-an-object-into-a-string

    // output is written to a byte array which is converted to a string
    val binArrayStream = new ByteArrayOutputStream()

    val objOutputSteam = new ObjectOutputStream(binArrayStream)

    objOutputSteam.writeObject(obj)

    objOutputSteam.close

    // returning generated String
    Base64.getEncoder.encodeToString(binArrayStream.toByteArray)
  }

  def stringToSerializable(str: String): Object = {//Serializable = {
    // given a string that is the serialization of an object, deserialize an return that object
    // TODO figure out how to cast to serializable if necessary
    // TODO error handling
    // via: https://stackoverflow.com/questions/134492/how-to-serialize-an-object-into-a-string

    val bytes = Base64.getDecoder.decode(str)

    val binArrayStream = new ByteArrayInputStream(bytes)

    val objInputStream = new ObjectInputStream(binArrayStream)

    val toReturn = objInputStream.readObject

    objInputStream.close

    // returning the generated obj
    toReturn
  }
}