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

  def flatIndexOf(s: String): Int

  def mapNames(m: (String) => String): Element
}

sealed abstract class ElementWithParent extends Element {
  def parent: String
}

sealed abstract class Primitive extends ElementWithParent {
  def flatIndexOf(s: String): Int = -1
}

sealed abstract class Group extends ElementWithParent {
  def elements: Seq[Option[String]]

  def flatIndexOf(s: String) = elements.indexOf(Some(s))
}

sealed abstract class Top extends Element {
  def topGroup: String
  def flatIndexOf(s: String): Int = if (topGroup == s) 0 else -1
}

////////////////////////////////////////////// Hierarchical barrier

private[floorplan] final case class HierarchicalBarrier(
  name: String,
  parent: String
) extends Primitive {
  final def level = 3
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
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
  def max = 4
}

sealed abstract class AbstractRectPrimitive extends Primitive {
  final def level = 4
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
) extends ConstrainedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

private[floorplan] final case class SizedSpacerRect(
  name: String,
  parent: String,
  x: BigDecimal,
  y: BigDecimal,
  width: BigDecimal,
  height: BigDecimal
) extends SizedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

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
) extends ConstrainedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

private[floorplan] final case class SizedLogicRect(
  name: String,
  parent: String,
  width: BigDecimal,
  height: BigDecimal,
  hardBoundary: Boolean
) extends SizedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

private[floorplan] final case class PlacedLogicRect(
  name: String,
  parent: String,
  x: BigDecimal,
  y: BigDecimal,
  width: BigDecimal,
  height: BigDecimal,
  hardBoundary: Boolean
) extends PlacedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}


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
  final def level = 3
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), topGroup = m(topGroup))
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
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), topGroup = m(topGroup))
}

////////////////////////////////////////////// Aggregate (Group) things

sealed abstract class Grid extends Group {
  def xDim: Int
  def yDim: Int
  def elements: Seq[Option[String]]

  assert(xDim > 0, "X dimension of grid must be positive")
  assert(yDim > 0, "Y dimension of grid must be positive")

  def get(x: Int, y: Int) = elements(xDim*y + x)

  def indexOf(s: String): Option[(Int, Int)] = {
    val idx = elements.indexOf(Some(s))
    val x = idx % xDim
    val y = idx / xDim
    if (idx == -1) None else Some((x, y))
  }
}

// TODO eventually rename this to AbstractWeightedGrid
private[floorplan] final case class WeightedGrid(
  name: String,
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]],
  weights: Seq[BigDecimal],
  packed: Boolean
) extends Grid {
  def level = 3
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
}

private[floorplan] final case class ConstrainedWeightedGrid(
  name: String,
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]],
  weights: Seq[BigDecimal],
  packed: Boolean,
  width: Seq[Constraint],
  height: Seq[Constraint],
  area: Seq[Constraint],
  aspectRatio: Seq[Constraint]
) extends Grid {
  def level = 2
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
}

// TODO eventually rename this to AbstractElasticGrid
private[floorplan] final case class ElasticGrid(
  name: String,
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]]
) extends Grid {
  def level = 3
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
}

private[floorplan] final case class ConstrainedElasticGrid(
  name: String,
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]],
  width: Seq[Constraint],
  height: Seq[Constraint],
  area: Seq[Constraint],
  aspectRatio: Seq[Constraint]
) extends Grid {
  def level = 2
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
}

private[floorplan] final case class SizedGrid(
  name: String,
  parent: String,
  xDim: Int,
  yDim: Int,
  elements: Seq[Option[String]],
  widths: Seq[BigDecimal],
  heights: Seq[BigDecimal]
) extends Grid {
  def level = 1
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
}


////////////////////////////////////////////// MemElements and Macros

// Reference to a MemElement
private[floorplan] final case class MemElement(
  name: String,
  parent: String
) extends AbstractRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

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
  def level = 4
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
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
  def mapNames(m: (String) => String): Element = {
    this.copy(
      name = m(name),
      parent = m(parent),
      elements = elements.map(_.map(m))
    )
  }
}

// Reference to a macro blackbox with unknown dimensions
// Do not use for SyncReadMem objects; use MemElement instead
private[floorplan] final case class AbstractMacro (
  name: String,
  parent: String
) extends AbstractRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

// Reference to a macro blackbox that has known dimensions
private[floorplan] final case class SizedMacro (
  name: String,
  parent: String,
  width: BigDecimal,
  height: BigDecimal
) extends SizedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

// Reference to a macro blackbox that has known dimensions and has been placed
private[floorplan] final case class PlacedMacro (
  name: String,
  parent: String,
  x: BigDecimal,
  y: BigDecimal,
  width: BigDecimal,
  height: BigDecimal
) extends PlacedRectPrimitive {
  def mapNames(m: (String) => String): Element = this.copy(name = m(name), parent = m(parent))
}

