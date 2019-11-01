// See LICENSE for license details
package barstools.floorplan

import org.json4s._
import org.json4s.native.Serialization.{read, write, writePretty}
import scala.reflect.runtime.universe.typeOf

final case class FloorplanElementRecord(path: Option[String], element: Element)

final case class FloorplanState(elements: Seq[FloorplanElementRecord], level: Int) {

  def generateDB: FloorplanState.Database = ???

}

object FloorplanSerialization {

  // Because Element is sealed, all of its subclasses are known at compile time, so we can construct type hints for them
  private val typeHintClasses = Seq(
      typeOf[Element],
      typeOf[Unit],
      typeOf[Constraint[Rational]],
      typeOf[Constraint[LengthUnit]],
      typeOf[Constraint[AreaUnit]],
    ).map(_.typeSymbol.asClass.knownDirectSubclasses.toList).reduce(_ ++ _)

  val formats = (new DefaultFormats {
    override val typeHintFieldName = "class"
    override val typeHints = FullTypeHints(typeHintClasses map { x => Class.forName(x.fullName) })
  })

  def serialize[T <: Element](elt: T): String = write(elt)(formats)

  def deserialize(str: String): Element = {
    // Not sure why the implicit doesn't work at the original definition, but the compiler complains
    implicit val formats = FloorplanSerialization.formats
    read[Element](str)
  }

}

object FloorplanState {

  type Database = Map[String, Element]

  def fromSeq(seq: Seq[FloorplanElementRecord]): FloorplanState = FloorplanState(seq, seq.map(_.element.level).max)

  def serialize(state: FloorplanState): String = writePretty(state)(FloorplanSerialization.formats)
  def serialize(seq: Seq[FloorplanElementRecord]): String = serialize(fromSeq(seq))

  def deserialize(str: String): FloorplanState = {
    implicit val formats = FloorplanSerialization.formats
    read[FloorplanState](str)
  }

}
