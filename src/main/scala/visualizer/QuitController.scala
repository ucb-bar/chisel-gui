// See LICENSE for license details.

package visualizer

import java.io.{File, IOException, PrintWriter}

import visualizer.models._
import visualizer.controllers.{BinFormat, DecFormat, Format, HexFormat}
import scalaswingcontrib.tree.{InternalTreeModel, Tree}

import scala.io.Source

//class QuitController(waveFormController: WaveFormController) {
class QuitController() {

  //def saveWaveformInfo(tree: Tree[SelectionNode], fileName: String) {
  def saveWaveformInfo(tree: Tree[SelectionNode], dataModel: SignalSelectionModel, fileName: String) {
    //var jsonResult = null

    var fileContents = ""

/*    // problem, dfs alone does not allow us to store a tree in a way that can be reconstructed
    TreeHelper.viewableDepthFirstIterator(tree).foreach {
      case signal: WaveSignal =>
        // serialize and output

        fileContents = fileContents + "\n" + signal.name
      case group: WaveGroup =>
        // serialize and output
        //println(s"Group ${signal.}")
      case _ =>
        // generate error
        // should not be any thing here, throw exception

    }*/

/*    tree.cellValues.foreach( node => {
      println(node.name)
    })*/

    //dataModel.roots.foreach { println }

    //TreeHelper.pathDepthFirstIterator(tree).map( {path => (pathToString(path), path.last)} ).foreach( {println})


    // pathDepthFirstIterator generates all the paths to nodes (one for each node in the tree)
    // each path is processed individually (map), then combined together into a large string (mkString)
    fileContents = TreeHelper.pathDepthFirstIterator(tree).map( {retData} ).mkString("\n")

    print(fileContents)

    // Storing the data on disk
    // TODO: determine necessary error trapping
    val file = new File(fileName)
    val pw = new PrintWriter(file)
    pw.write(fileContents)
    pw.close()

  }

  // tree: an empty tree which we will write our data to following a load
  // fileName: the file on disk to read the tree from
  def readWaveformInfo(fileName: String, tree: Tree[SelectionNode]): Unit = {

    // read the file
    // TODO: add error trapping for file not existing, etc.
    // if there is an error we should clear the file on disk before exiting (otherwise user might continuously get err)

    val file = Source.fromFile(fileName)

    if (file.isEmpty) {
      println("QuitController: No saved Waveform data was found.")
      return
    }

    try {
      // reading the file in line by line
      val signals = file.getLines().map( {processData} )

      for (signal <- signals) {
        println(signal)

        val parents = decomposePathStr(signal.last)
        println(s"Parents: $parents")

      }

    } catch {
      case ioEx: IOException =>
        println("IOException encountered when attempting to load Waveform preferences from disk.")
        ioEx.printStackTrace()
      case ex: Exception => ex.printStackTrace()
    } finally {

    }

  }

  // Utility Methods

  // The string used to separate different data items on disk
  val diskDelimiter = ","

  // The string of characters used to separate the nodes in a path in the file stored on disk
  // This is shared between all methods that deal with saving or reading paths from disk
  val pathSplitter = "/"

  def pathToString(path: Seq[SelectionNode]): String = {
    path.map({node => node.name}).mkString(pathSplitter)
  }


  def decomposePathStr(path: String): Seq[String] = {
    path.split(pathSplitter)
  }


  // Given a path of nodes, returns a string composed of [ state data for last node on path, path] to be stored on disk
  // This method works on all types of SelectionNodes because of the addition of getStateData to the SelectionNode trait
  private def retData(path: Seq[SelectionNode]): String = {
    if (path == null) {
      return ""
    }
    (path.last.getStateData :+ pathToString(path)).mkString(diskDelimiter)
  }


  // Given a string line stored on disk, extracts the useful information from the said line
  // This is called when reading the saved data of disk
  // this can be thought of as the inverse of "retData"
  private def processData(line: String): Seq[String] = {
    //val items = line.split(diskDelimiter)

//    for (item <- items) {
//     print(s" -> $item")
//    }
//    println

    //val signalName = items(0)

    return line.split(diskDelimiter)
  }
  


}
