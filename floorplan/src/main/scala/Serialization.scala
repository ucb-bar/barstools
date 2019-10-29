// See LICENSE for license details
package barstools.floorplan

import org.json4s._
import org.json4s.native.Serialization.{read, write}
import scala.reflect.runtime.universe.typeOf

final case class FloorplanElementRecord[T <: Element](path: String, element: T)

final case class FloorplanState(elements: Seq[FloorplanElementRecord[Element]], level: Int)

object FloorplanSerialization {

  private val elements = typeOf[Element].typeSymbol.asClass.knownDirectSubclasses.toList

  // Because Element is sealed, all of its subclasses are known at compile time, so we can construct type hints for them
  val formats = new DefaultFormats {
    override val typeHintFieldName = "class"
    override val typeHints = FullTypeHints(elements map { x => Class.forName(x.fullName) })
  }

  def serialize[T <: Element](elt: T): String = write(elt)(formats)

  def deserialize(str: String): Element = {
    // Not sure why the implicit doesn't work at the original definition, but the compiler complains
    implicit val formats = FloorplanSerialization.formats
    read[Element](str)
  }

}

object FloorplanState {

  def fromSeq(seq: Seq[FloorplanElementRecord[Element]]): FloorplanState = FloorplanState(seq, seq.map(_.element.level).max)

  def serialize(state: FloorplanState): String = write(state)(FloorplanSerialization.formats)
  def serialize(seq: Seq[FloorplanElementRecord[Element]]): String = serialize(fromSeq(seq))

  def deserialize(str: String): FloorplanState = {
    implicit val formats = FloorplanSerialization.formats
    read[FloorplanState](str)
  }

}
