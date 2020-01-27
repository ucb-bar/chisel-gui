// See README.md for license details.

package visualizer.stage

import java.io.File

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, Shell, ShellOption, Unserializable}
import firrtl.stage.FirrtlCli

trait ChiselGuiOption extends Annotation with Unserializable

trait ChiselGuiCli extends FirrtlCli {
  this: Shell =>
  parser.note("ChiselGui Options")
  Seq(
    ChiselSourcePaths,
    ChiselSourceOpenCommand,
    VcdFile,
    PrimaryClockName
  ).foreach(_.addOptions(parser))
}

object ChiselSourceOpenCommand extends HasShellOptions {
  override def options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "chisel-source-open-command",
      toAnnotationSeq = a => Seq(ChiselSourceOpenCommand(a)),
      helpText = s"A sequence of strings, that together" +
        s" are the command to open the chisel source associated with a signal\n" +
        s"Use [[FILE]] and [[LINE]] (case matters) as substitution points for the file and for the line number to jump to"
    )
  )
}

case class ChiselSourceOpenCommand(paths: Seq[String]) extends NoTargetAnnotation with ChiselGuiOption

object ChiselSourcePaths extends HasShellOptions {
  override def options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[File]](
      longOption = "chisel-source-paths",
      toAnnotationSeq = a => Seq(ChiselSourcePaths(a)),
      helpText = "A comma separated list of directories where chisel scala files can be found",
    )
  )
}

case class ChiselSourcePaths(paths: Seq[File]) extends NoTargetAnnotation with ChiselGuiOption

object VcdFile extends HasShellOptions {
  override def options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd",
      toAnnotationSeq = a => Seq(VcdFile(a)),
      helpText = "vcd data for waveform viewer",
    )
  )
}

case class VcdFile(vcdFileName: String) extends NoTargetAnnotation with ChiselGuiOption

object PrimaryClockName extends HasShellOptions {
  override def options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "vcd-file",
      toAnnotationSeq = a => Seq(PrimaryClockName(a)),
      helpText = "vcd data for waveform viewer",
    )
  )
}

case class PrimaryClockName(primaryClockName: String) extends NoTargetAnnotation with ChiselGuiOption

case object AggegrateReadyValid extends HasShellOptions with NoTargetAnnotation with ChiselGuiOption {
  override def options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "dont-ready-valid",
      toAnnotationSeq = a => Seq(AggegrateReadyValid),
      helpText = "defeats default aggregation of decoupled bundles",
    )
  )
}
