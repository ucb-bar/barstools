// See LICENSE for license details
package barstools.floorplan

import java.io.{File, FileWriter}

final case class FloorplanElementRecord(root: String, inst: Option[String], element: Element, memExt: Option[String] = None)

final case class FloorplanState(records: Seq[FloorplanElementRecord], level: Int)

object FloorplanState {

  def fromSeq(seq: Seq[FloorplanElementRecord]): FloorplanState = FloorplanState(seq, (Seq(IRLevel.max) ++ seq.map(_.element.level)).max)

  def serialize(state: FloorplanState): String = FloorplanSerialization.serializeState(state)
  def serialize(seq: Seq[FloorplanElementRecord]): String = serialize(fromSeq(seq))

  def deserialize(str: String): FloorplanState = FloorplanSerialization.deserializeState(str)

  def fromFile(file: String): FloorplanState = {
    val source = scala.io.Source.fromFile(file)
    val fpState = deserialize(source.getLines.mkString("\n"))
    source.close()
    fpState
  }

  def toFile(file: String, fmt: OutputFormat, state: FloorplanState) {
    val writer = new FileWriter(new File(file))
    fmt match {
      case OutputFormat.HammerIR => writer.write(FloorplanState.toHammerIR(state))
      case OutputFormat.FloorplanIR => writer.write(FloorplanState.serialize(state))
    }
    writer.close()
  }

  def fromFiles(files: Seq[String]): FloorplanState = {
    assert(files.length == 1, "FIXME combine multiple states into one")
    fromFile(files(0))
  }

  def toHammerIR(state: FloorplanState): String = {
    assert(state.level == 0, "Can only convert level 0 FloorplanState")
    ???
  }
}

sealed trait OutputFormat

object OutputFormat {
  case object HammerIR extends OutputFormat
  case object FloorplanIR extends OutputFormat
}

