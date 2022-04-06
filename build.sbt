// See LICENSE for license details.

val defaultVersions = Map(
  "chisel3" -> "3.5.1",
  "chisel-iotesters" -> "2.5.1"
)

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version := "0.4-SNAPSHOT",
  scalaVersion := "2.12.10",
  scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls", "-Xsource:2.11"),
  libraryDependencies ++= Seq("chisel3","chisel-iotesters").map {
    dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
  },
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    "org.json4s" %% "json4s-jackson" % "3.6.1",
    "org.json4s" %% "json4s-native" % "3.6.1",
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  )
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
