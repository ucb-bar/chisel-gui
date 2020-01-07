// See README.md for license details.

package visualizer

import java.io.File

class SourceFinder(paths: Seq[File]) {

}

object SourceFinder {
  def walk(f: File): Seq[File] = {
    if (f.exists()) {
      if (!f.isDirectory) {
        if (f.getName.endsWith(".scala")) {
          Seq(f)
        } else {
          Seq.empty
        }
      } else {
        f.listFiles().flatMap { ff =>
          walk(ff)
        }
      }
    } else {
      Seq.empty
    }
  }

  def main(args: Array[String]): Unit = {
    val scalaFiles = args.map(n => new File(n)).flatMap { f =>
      walk(f)
    }

    scalaFiles.foreach { f => println(f.getName -> f.getAbsolutePath) }
  }
}
