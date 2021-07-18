// See LICENSE for license details
package barstools.floorplan

import scala.math.{BigInt, BigDecimal}

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

sealed abstract class ElementWithParent extends Element {
  def parent: String
}

sealed abstract class Primitive extends ElementWithParent

sealed abstract class Group extends ElementWithParent {
  def elements: Seq[Option[String]]
}

sealed abstract class Top extends Element

////////////////////////////////////////////// Hierarchical barrier

private[floorplan] final case class HierarchicalBarrier(
  name: String,
  parent: String
) extends Primitive {
  final def level = 0
}

////////////////////////////////////////////// Rectangular things

sealed trait ConstrainedRectLike {
  def width: Constraint
  def height: Constraint
  def area: Constraint
  def aspectRatio: Constraint
}

sealed trait SizedRectLike {
  def width: BigDecimal
  def height: BigDecimal
}

sealed trait PlacedRectLike {
  def x: BigDecimal
  def y: BigDecimal
  def width: BigDecimal
  def height: BigDecimal
}

object IRLevel {
  def max = 3
}

sealed abstract class AbstractRectPrimitive extends Primitive {
  final def level = 3
}

sealed abstract class ConstrainedRectPrimitive extends Primitive with ConstrainedRectLike {
  final def level = 2
}

sealed abstract class SizedRectPrimitive extends Primitive with SizedRectLike {
  final def level = 1
}

sealed abstract class PlacedRectPrimitive extends Primitive with PlacedRectLike {
  final def level = 0
}

private[floorplan] final case class ConstrainedSpacerRect(
  name: String,
  parent: String,
  width: Constraint = Unconstrained(),
  height: Constraint = Unconstrained(),
  area: Constraint = Unconstrained(),
  aspectRatio: Constraint = Unconstrained()
) extends ConstrainedRectPrimitive

private[floorplan] final case class SizedSpacerRect(
  name: String,
  parent: String,
  x: BigDecimal,
  y: BigDecimal,
  width: BigDecimal,
  height: BigDecimal
) extends SizedRectPrimitive

// No PlacedSpacerRect exists because they're only for spacing

/////////////////////////// Hierarchical top modules

case class Margins(left: BigDecimal, right: BigDecimal, top: BigDecimal, bottom: BigDecimal)

object Margins {
  def empty = Margins(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(0))
}

private[floorplan] final case class ConstrainedLogicRect(
  name: String,
  parent: String,
  width: Constraint,
  height: Constraint,
  area: Constraint,
  aspectRatio: Constraint,
  hardBoundary: Boolean
) extends ConstrainedRectPrimitive

private[floorplan] final case class SizedLogicRect(
  name: String,
  parent: String,
  width: BigDecimal,
  height: BigDecimal,
  hardBoundary: Boolean
) extends SizedRectPrimitive

private[floorplan] final case class PlacedLogicRect(
  name: String,
  parent: String,
  x: BigDecimal,
  y: BigDecimal,
  width: BigDecimal,
  height: BigDecimal,
  hardBoundary: Boolean
) extends PlacedRectPrimitive


private[floorplan] final case class ConstrainedHierarchicalTop(
  name: String,
  topGroup: String,
  width: Constraint,
  height: Constraint,
  area: Constraint,
  aspectRatio: Constraint,
  margins: Margins,
  hardBoundary: Boolean
) extends Top with ConstrainedRectLike {
  final def level = 2
}

private[floorplan] final case class PlacedHierarchicalTop(
  name: String,
  topGroup: String,
  width: BigDecimal,
  height: BigDecimal,
  margins: Margins,
  hardBoundary: Boolean
) extends Top with PlacedRectLike {
  final def x = BigDecimal(0)
  final def y = BigDecimal(0)
  final def level = 0
}

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
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]],
  weights: Seq[BigDecimal],
  packed: Boolean
) extends Grid {
  def level = 1
}

private[floorplan] final case class ElasticGrid(
  name: String,
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]]
) extends Grid {
  def level = 1
}


////////////////////////////////////////////// MemElements and Macros

// Reference to a MemElement
private[floorplan] final case class MemElement(
  name: String,
  parent: String
) extends AbstractRectPrimitive

// Container for MemElements
private[floorplan] final case class MemElementArray(
  name: String,
  parent: String,
  elements: Seq[Option[String]],
  width: Constraint = Unconstrained(),
  height: Constraint = Unconstrained(),
  area: Constraint = Unconstrained(),
  aspectRatio: Constraint = Unconstrained()
) extends Group with ConstrainedRectLike {
  def level = 3
}

// Container for MemElements that have been converted to Macros
// This may be unnecessary, but the purpose of this is to let the floorplan
// tool treat memory macros differently than generic macros, since they
// are more commonly arrayed
private[floorplan] final case class MemMacroArray(
  name: String,
  parent: String,
  elements: Seq[Option[String]],
  width: Constraint = Unconstrained(),
  height: Constraint = Unconstrained(),
  area: Constraint = Unconstrained(),
  aspectRatio: Constraint = Unconstrained()
) extends Group with ConstrainedRectLike {
  def level = 2
}

// Reference to a macro blackbox with unknown dimensions
// Do not use for SyncReadMem objects; use MemElement instead
private[floorplan] final case class AbstractMacro (
  name: String,
  parent: String
) extends AbstractRectPrimitive

// Reference to a macro blackbox that has known dimensions
private[floorplan] final case class SizedMacro (
  name: String,
  parent: String,
  width: BigDecimal,
  height: BigDecimal
) extends SizedRectPrimitive

// Reference to a macro blackbox that has known dimensions and has been placed
private[floorplan] final case class PlacedMacro (
  name: String,
  parent: String,
  x: BigDecimal,
  y: BigDecimal,
  width: BigDecimal,
  height: BigDecimal
) extends PlacedRectPrimitive

