name := "visualizer"

version := "0.1"

scalaVersion := "2.11.12"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("public")
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
  "edu.berkeley.cs" %% "treadle" % "1.1-SNAPSHOT"
)