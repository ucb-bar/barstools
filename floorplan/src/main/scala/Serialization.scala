// See LICENSE for license details
package barstools.floorplan

import org.json4s._
import org.json4s.native.Serialization.{read, write}
import scala.reflect.runtime.universe.typeOf

object FloorplanSerialization {

  // Because Element is sealed, all of its subclasses are known at compile time, so we can construct type hints for them
  val formats = new DefaultFormats {
    override val typeHintFieldName = "class"
    override val typeHints = FullTypeHints(typeOf[Element].
      typeSymbol.
      asClass.
      knownDirectSubclasses.
      toList.
      map { x => Class.forName(x.fullName) })
  }

  def serialize[T <: Element](elt: T): String = write(elt)(formats)
  def serialize[T <: Element](elts: Seq[(String, T)]): String = write(elts)(formats)

  def deserialize(fpir: String): Element = {
    // Not sure why the implicit doesn't work at the original definition, but the compiler complains
    implicit val formats = FloorplanSerialization.formats
    read(fpir)
  }

}
