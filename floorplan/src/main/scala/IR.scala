// See LICENSE for license details
package barstools.floorplan

sealed abstract class IRLevel(val level: Int) {
  // TODO
}

case object IRLevel0 extends IRLevel(0)
case object IRLevel1 extends IRLevel(1)
case object IRLevel2 extends IRLevel(2)


////////////////////////////////////////////// Base classes

sealed abstract class Element {
  val name: String

  def level: IRLevel
  def serialize = FloorplanSerialization.serialize(this)

}

sealed abstract class Primitive extends Element

////////////////////////////////////////////// Rect shape

sealed abstract class AbstractRect extends Primitive {
  final def level = IRLevel2
}

sealed abstract class ConstrainedRect extends Primitive {
  final def level = IRLevel1

  val width: Constraint[LengthUnit]
  val height: Constraint[LengthUnit]
  val area: Constraint[AreaUnit]
  val aspectRatio: Constraint[Unitless]

}

sealed abstract class ConcreteRect extends Primitive {
  final def level = IRLevel0

  val width: LengthUnit
  val height: LengthUnit

}

sealed trait MacroLike

final case class AbstractMacro(name: String) extends AbstractRect with MacroLike

final case class ConcreteMacro(name: String, width: LengthUnit, height: LengthUnit) extends ConcreteRect with MacroLike

sealed trait LogicLike {
  val hardBoundary: Boolean
}

final case class AbstractLogicRect(name: String, hardBoundary: Boolean) extends AbstractRect with LogicLike {

  def constrain(
    width: Constraint[LengthUnit] = UnconstrainedLength,
    height: Constraint[LengthUnit] = UnconstrainedLength,
    area: Constraint[AreaUnit] = UnconstrainedArea,
    aspectRatio: Constraint[Unitless] = UnconstrainedUnitless) = ConstrainedLogicRect(name, width, height, area, aspectRatio, hardBoundary)

}

final case class ConstrainedLogicRect(
  name: String,
  width: Constraint[LengthUnit],
  height: Constraint[LengthUnit],
  area: Constraint[AreaUnit],
  aspectRatio: Constraint[Unitless],
  hardBoundary: Boolean) extends ConstrainedRect with LogicLike

final case class ConcreteLogicRect(name: String, width: LengthUnit, height: LengthUnit, hardBoundary: Boolean) extends ConcreteRect with LogicLike

sealed abstract class Group extends Element

sealed abstract class Grid extends Group {
  val xDim: Int
  val yDim: Int
  val elements: Seq[Element]

  assert(xDim > 0, "X dimension of grid must be positive")
  assert(yDim > 0, "Y dimension of grid must be positive")

  def get(x: Int, y: Int) = elements(xDim*y + x)
}

final case class AbstractPackedHomogenousGrid[T <: Element](name: String, xDim: Int, yDim: Int, elements: Seq[T]) extends Grid {
  def level = IRLevel2
}

object FloorplanSerialization {

  import org.json4s._
  import org.json4s.native.Serialization.{read, write}
  import scala.reflect.runtime.universe.typeOf

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

}
