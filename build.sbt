
name := "chisel-gui"

organization := "edu.berkeley.cs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.12.6", "2.11.12")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("public")
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
  "edu.berkeley.cs" %% "treadle" % "1.2-SNAPSHOT",
  "com.github.benhutchison" %% "scalaswingcontrib" % "1.7"
)

// Assembly

assemblyJarName in assembly := "chisel-gui.jar"

mainClass in assembly := Some("visualizer.AppController")

test in assembly := {} // Should there be tests?

assemblyOutputPath in assembly := file("./utils/bin/chisel-gui.jar")

