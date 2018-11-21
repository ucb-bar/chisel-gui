package utilities

import org.scalatest.{FreeSpec, Matchers}
import scalaswingcontrib.tree.Tree
import treadle.TreadleTester
import visualizer.QuitController
import visualizer.models.{SelectionGroup, SelectionNode, SignalSelectionModel}

class QuitControllerTest extends FreeSpec with Matchers {
  "can store and load a generic tree" in {
    // making a test tree
    val testTree = new Tree[SelectionNode]




    val qTest = new QuitController


    // Writing our tree to a file on disk
    //qTest.saveWaveformInfo()

    // Reading our data from disk

    // checking the data



  }

  val ioTestFilePath = "IOTest/test.txt"

  "basic test of file writing" in {

    // building a tree
    //val testTree = new Tree[SelectionNode]






    val qTest = new QuitController
    qTest.saveWaveformInfo(null, ioTestFilePath)

  }



  "basic test of reading after writing" in {
    val qTest = new QuitController
    qTest.readWaveformInfo(ioTestFilePath, null)

  }
}