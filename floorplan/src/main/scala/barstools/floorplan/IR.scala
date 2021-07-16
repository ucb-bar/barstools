// See LICENSE for license details
package barstools.floorplan

// TODO Make some of this stuff private
// TODO make level an enum
// TODO add versioning to the FPIR file
// TODO clean up comment syntax, add scaladoc

////////////////////////////////////////////// Base classes

sealed abstract class Element {
  def name: String
  def level: Int
  def serialize = FloorplanSerialization.serialize(this)
}

sealed abstract class Primitive extends Element

sealed abstract class Group extends Element {
  def elements: Seq[Option[String]]
}

////////////////////////////////////////////// Hierarchical barrier

private[floorplan] final case class HierarchicalBarrier(
  name: String
) extends Primitive {
  final def level = 0 // Maybe 1
}

////////////////////////////////////////////// Rectangular things

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

object IRLevel {
  def max = 2
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

private[floorplan] final case class ConstrainedLogicRect(
  name: String,
  width: Constraint[LengthUnit],
  height: Constraint[LengthUnit],
  area: Constraint[AreaUnit],
  aspectRatio: Constraint[Rational],
  hardBoundary: Boolean
) extends ConstrainedRectPrimitive

private[floorplan] final case class ConcreteLogicRect(
  name: String,
  width: LengthUnit,
  height: LengthUnit,
  hardBoundary: Boolean
) extends ConcreteRectPrimitive

////////////////////////////////////////////// Aggregate (Group) things

sealed abstract class Grid extends Group {
  def xDim: Int
  def yDim: Int
  def elements: Seq[Option[String]]

  assert(xDim > 0, "X dimension of grid must be positive")
  assert(yDim > 0, "Y dimension of grid must be positive")

  def get(x: Int, y: Int) = elements(xDim*y + x)
}

private[floorplan] final case class WeightedGrid(
  name: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]],
  weights: Seq[Rational],
  packed: Boolean
) extends Grid {
  def level = 1
}


////////////////////////////////////////////// MemElements and Macros

// Reference to a MemElement
private[floorplan] final case class MemElement(
  name: String
) extends AbstractRectPrimitive

// Container for MemElements
private[floorplan] final case class MemElementArray(
  name: String,
  elements: Seq[Option[String]],
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends Group with ConstrainedRectLike {
  def level = 2
}

// Container for MemElements that have been converted to Macros
// This may be unnecessary, but the purpose of this is to let the floorplan
// tool treat memory macros differently than generic macros, since they
// are more commonly arrayed
private[floorplan] final case class MemMacroArray(
  name: String,
  elements: Seq[Option[String]],
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends Group with ConstrainedRectLike {
  def level = 1
}

// Reference to a macro blackbox with unknown dimensions
// Do not use for SyncReadMem objects; use MemElement instead
private[floorplan] final case class AbstractMacro (
  name: String
) extends AbstractRectPrimitive

// Reference to a macro blackbox that has known dimensions
private[floorplan] final case class ConcreteMacro (
  name: String,
  width: LengthUnit,
  height: LengthUnit
) extends ConcreteRectPrimitive

