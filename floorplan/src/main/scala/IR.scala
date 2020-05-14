// See LICENSE for license details
package barstools.floorplan

// TODO Make some of this stuff private

////////////////////////////////////////////// Base classes

sealed abstract class Element {

  def name: String
  // TODO make this an Enumeration
  def level: Int
  def serialize = FloorplanSerialization.serialize(this)

}

sealed abstract class Primitive extends Element

////////////////////////////////////////////// Rect shape

trait ConstrainedRectLike {
  def width: Constraint[LengthUnit]
  def height: Constraint[LengthUnit]
  def area: Constraint[AreaUnit]
  def aspectRatio: Constraint[Rational]
}

trait ConcreteRectLike {
  def width: LengthUnit
  def height: LengthUnit
}

sealed abstract class AbstractRectPrimitive extends Primitive {
  final def level = 2
}

sealed abstract class ConstrainedRectPrimitive extends Primitive with ConstrainedRectLike {
  final def level = 1
}

sealed abstract class ConcreteRectPrimitive extends Primitive with ConcreteRectLike {
  final def level = 0
}

private[floorplan] final case class ConstrainedDummyRect(
  name: String,
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ConstrainedRectPrimitive

sealed trait MacroLike

private[floorplan] final case class AbstractMacro(name: String) extends AbstractRectPrimitive with MacroLike

private[floorplan] final case class ConcreteMacro(name: String, width: LengthUnit, height: LengthUnit) extends ConcreteRectPrimitive with MacroLike

private[floorplan] final case class ConstrainedLogicRect(
  width: Constraint[LengthUnit],
  height: Constraint[LengthUnit],
  area: Constraint[AreaUnit],
  aspectRatio: Constraint[Rational],
  hardBoundary: Boolean
) extends ConstrainedRectPrimitive {
  def name = ""
}

private[floorplan] final case class ConcreteLogicRect(width: LengthUnit, height: LengthUnit, hardBoundary: Boolean) extends ConcreteRectPrimitive {
  def name = ""
}

sealed abstract class Group extends Element {

  def elements: Seq[String]

  def mapElements(f: ((String, Int)) => String): Group

  protected def mapElementsHelper(f: ((String, Int)) => String): Seq[String] = elements.zipWithIndex.map(f)

}

sealed abstract class Grid extends Group {
  def xDim: Int
  def yDim: Int
  def elements: Seq[String]

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

  def mapElements(f: ((String, Int)) => String) = this.copy(name, xDim, yDim, mapElementsHelper(f), weights, packed)

}

/*
sealed abstract class Layout extends Group
sealed abstract class ConstrainedLayout extends Layout with ConstrainedRectLike
sealed abstract class ConcreteLayout extends Layout with ConcreteRectLike

private[floorplan] final case class ConstrainedRatioLayout(
  elements: Seq[String],
  placements: Seq[RatioPlacementConstraint],
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ConstrainedLayout {

  def level = 1

  def mapElements(f: ((String, Int)) => String) = this.copy(mapElementsHelper(f), placements, width, height, area, aspectRatio)

}

private[floorplan] final case class ConstrainedLengthLayout(
  elements: Seq[String],
  placements: Seq[LengthPlacementConstraint],
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ConstrainedLayout {

  def level = 1

  def mapElements(f: ((String, Int)) => String) = this.copy(mapElementsHelper(f), placements, width, height, area, aspectRatio)

}

*/

// TODO add more layouts here
