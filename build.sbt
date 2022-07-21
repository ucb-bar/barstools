// See LICENSE for license details.

val defaultVersions = Map(
  "chisel3" -> "3.5.3",
  "chisel-iotesters" -> "2.5.1"
)

organization := "edu.berkeley.cs"
//version := "0.5-SNAPSHOT"
name := "barstools"
scalaVersion := "2.12.15"
crossScalaVersions := Seq("2.12.15", "2.13.8")
scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls")
Test / scalacOptions ++= Seq("-language:reflectiveCalls")
fork := true
mainClass := Some("barstools.macros.MacroCompiler")
libraryDependencies ++= Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
}
libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.2",
  "org.apache.logging.log4j" % "log4j-api" % "2.18.0",
  "org.apache.logging.log4j" % "log4j-core" % "2.18.0",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % defaultVersions("chisel3") cross CrossVersion.full)
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal
)
enablePlugins(GitVersioning)
