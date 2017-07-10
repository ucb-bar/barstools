// See LICENSE for license details.

import Dependencies._

resolvers in ThisBuild ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls"),
  libraryDependencies ++= commonDependencies
)

val defaultVersions = Map(
  "chisel3" -> "3.0-SNAPSHOT",
  "chisel-iotesters" -> "1.1-SNAPSHOT"
)

val firrtl_path = sys.props.getOrElse("FIRRTL_HOME", default="firrtl")
lazy val firrtl = RootProject(file(firrtl_path))

lazy val tapeout = (project in file("tapeout"))
  .dependsOn(firrtl)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq("chisel3","chisel-iotesters").map {
      dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
    }
  )
  .settings(scalacOptions in Test ++= Seq("-language:reflectiveCalls"))
