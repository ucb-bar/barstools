// See LICENSE for license details
package barstools.floorplan

final case class FloorplanElementRecord(root: String, inst: Option[String], element: Element, memExt: Option[String] = None)

final case class FloorplanState(elements: Seq[FloorplanElementRecord], level: Int)

object FloorplanState {

  def fromSeq(seq: Seq[FloorplanElementRecord]): FloorplanState = FloorplanState(seq, (Seq(IRLevel.max) ++ seq.map(_.element.level)).max)

  def serialize(state: FloorplanState): String = FloorplanSerialization.serializeState(state)
  def serialize(seq: Seq[FloorplanElementRecord]): String = serialize(fromSeq(seq))

  def deserialize(str: String): FloorplanState = FloorplanSerialization.deserializeState(str)

}
