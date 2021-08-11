// See LICENSE for license details
package barstools.floorplan

import org.json4s._
import org.json4s.native.Serialization.{read, write, writePretty}
import org.json4s.ext.EnumNameSerializer
import scala.reflect.runtime.universe.typeOf

object FloorplanSerialization {

  // Because Element is sealed, all of its subclasses are known at compile time, so we can construct type hints for them
  private val typeHintClasses = Seq(
      typeOf[Element],
      typeOf[Unit],
      typeOf[PlacementAnchor],
      typeOf[Constraint],
    ).map(_.typeSymbol.asClass.knownDirectSubclasses.toList).reduce(_ ++ _)

  val formats = (new DefaultFormats {
    override val typeHintFieldName = "class"
    override val typeHints = FullTypeHints(typeHintClasses map { x => Class.forName(x.fullName) })
  } + new EnumNameSerializer(Orientation))

  def serialize[T <: Element](elt: T): String = write(elt)(formats)

  def deserialize(str: String): Element = {
    // Not sure why the implicit doesn't work at the original definition, but the compiler complains
    implicit val formats = FloorplanSerialization.formats
    read[Element](str)
  }

  def serializeState(state: FloorplanState): String = writePretty(state)(formats)

  def deserializeState(str: String): FloorplanState = {
    implicit val formats = FloorplanSerialization.formats
    read[FloorplanState](str)
  }

}
