// See LICENSE for license details
package barstools.floorplan

// TODO Make some of this stuff private

////////////////////////////////////////////// Base classes

sealed abstract class Element {

  val name: String

  // TODO make this an Enumeration
  def level: Int
  def serialize = FloorplanSerialization.serialize(this)

}

sealed abstract class Primitive extends Element

////////////////////////////////////////////// Rect shape

sealed abstract class AbstractRect extends Primitive {
  final def level = 2
}

sealed abstract class ConstrainedRect extends Primitive {
  final def level = 1

  val width: Constraint[LengthUnit]
  val height: Constraint[LengthUnit]
  val area: Constraint[AreaUnit]
  val aspectRatio: Constraint[Rational]

}

sealed abstract class ConcreteRect extends Primitive {
  final def level = 0

  val width: LengthUnit
  val height: LengthUnit

}

private[floorplan] final case class ConstrainedPlaceholderRect(
  name: String,
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]) extends ConstrainedRect

sealed trait MacroLike

private[floorplan] final case class AbstractMacro(name: String) extends AbstractRect with MacroLike

private[floorplan] final case class ConcreteMacro(name: String, width: LengthUnit, height: LengthUnit) extends ConcreteRect with MacroLike

sealed trait LogicLike {
  val hardBoundary: Boolean
}

private[floorplan] final case class ConstrainedLogicRect(
  name: String,
  width: Constraint[LengthUnit],
  height: Constraint[LengthUnit],
  area: Constraint[AreaUnit],
  aspectRatio: Constraint[Rational],
  hardBoundary: Boolean) extends ConstrainedRect with LogicLike

private[floorplan] final case class ConcreteLogicRect(name: String, width: LengthUnit, height: LengthUnit, hardBoundary: Boolean) extends ConcreteRect with LogicLike

sealed abstract class Group extends Element

sealed abstract class Grid extends Group {
  val xDim: Int
  val yDim: Int
  val elements: Seq[String]

  assert(xDim > 0, "X dimension of grid must be positive")
  assert(yDim > 0, "Y dimension of grid must be positive")

  def get(x: Int, y: Int) = elements(xDim*y + x)
}

private[floorplan] final case class WeightedGrid(
  name: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[String],
  weights: Seq[Rational],
  packed: Boolean
) extends Grid {
  def level = 1
}

// TODO add more layouts here
