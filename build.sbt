// See LICENSE for license details.

enablePlugins(GitVersioning)
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % defaultVersions("chisel3") cross CrossVersion.full)

val defaultVersions = Map(
  "chisel3" -> "3.5.3",
  "chisel-iotesters" -> "2.5.1"
)

lazy val buildSettings = Seq(
  organization := "edu.berkeley.cs",
  // version computed by sbt-ci-release-early
  name := "barstools",
  scalaVersion := "2.12.15",
  crossScalaVersions := Seq("2.12.15", "2.13.8"),
  scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls"),

  Test / scalacOptions ++= Seq("-language:reflectiveCalls"),
  fork := true,

  mainClass := Some("barstools.macros.MacroCompiler"),

  libraryDependencies ++= (Seq("chisel3","chisel-iotesters").map {
    dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
  }),

  libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play-json" % "2.9.2",
    "org.apache.logging.log4j" % "log4j-api" % "2.18.0",
    "org.apache.logging.log4j" % "log4j-core" % "2.18.0",
    "org.scalatest" %% "scalatest" % "3.2.9" % "test"
  ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  )
)

lazy val publishSettings = Seq(
  scmInfo := Some(ScmInfo(
      url("https://github.com/ucb-bar/barstools"),
      "scm:git@github.com:ucb-bar/barstools.git")),
  licenses := List("BSD 3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  developers := List(Developer("edwardcwang", "Edward Wang", "", url("https://github.com/edwardcwang")), Developer("chick", "Chick Markley", "", url("https://github.com/chick"))),
  homepage := Some(url("https://github.com/ucb-bar/barstools/")),
  publishTo := sonatypePublishToBundle.value,

  Test / publishArtifact := false,
  publishMavenStyle := true,
  publish / skip := true // disable publishing until a tag is properly set via another PR
)

lazy val barstools = (project in file("."))
  .settings(buildSettings)
  .settings(publishSettings)
