// See LICENSE for license details.

val defaultVersions = Map(
  "chisel3" -> "3.5.1",
  "chisel-iotesters" -> "2.5.1"
)

organization := "edu.berkeley.cs"
version := "0.4-SNAPSHOT"
name := "tapeout"
scalaVersion := "2.12.13"
crossScalaVersions := Seq("2.12.13", "2.13.6")
scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls")
Test / scalacOptions ++= Seq("-language:reflectiveCalls")
fork := true
mainClass := Some("barstools.macros.MacroCompiler")
libraryDependencies ++= Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
}
libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.2",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.apache.logging.log4j" % "log4j-api" % "2.11.2",
  "org.apache.logging.log4j" % "log4j-core" % "2.11.2"
)
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % defaultVersions("chisel3") cross CrossVersion.full)
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal
)

disablePlugins(sbtassembly.AssemblyPlugin)

lazy val mdf = (project in file("mdf/scalalib"))
lazy val macros = (project in file("macros"))
  .dependsOn(mdf)
  .settings(commonSettings)
  .settings(Seq(
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "firrtl-interpreter" % "1.2-SNAPSHOT" % Test
    ),
    mainClass := Some("barstools.macros.MacroCompiler")
  ))
  .enablePlugins(sbtassembly.AssemblyPlugin)

lazy val tapeout = (project in file("tapeout"))
  .settings(commonSettings)
  .settings(Seq(
    libraryDependencies ++= Seq(
      "io.github.daviddenton" %% "handlebars-scala-fork" % "2.3.0"
    )
  ))
  .settings(scalacOptions in Test ++= Seq("-language:reflectiveCalls"))

lazy val floorplan = (project in file("floorplan"))
  .settings(commonSettings)

lazy val root = (project in file(".")).aggregate(macros, tapeout, floorplan)
