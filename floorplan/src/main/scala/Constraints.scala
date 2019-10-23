// See LICENSE for license details
package barstools.floorplan

sealed trait Unit

// TODO how to cleanly round down when dividing?
final case class Unitless(value: BigInt) extends Unit {
  def *(other: Unitless) = Unitless(this.value * other.value)
  def /(other: Unitless) = Unitless(this.value / other.value)
  def +(other: Unitless) = Unitless(this.value + other.value)
  def -(other: Unitless) = Unitless(this.value - other.value)

  def *(other: LengthUnit) = LengthUnit(this.value * other.value)
  def /(other: LengthUnit) = LengthUnit(this.value / other.value)
  def +(other: LengthUnit) = LengthUnit(this.value + other.value)
  def -(other: LengthUnit) = LengthUnit(this.value - other.value)

  def *(other: AreaUnit) = AreaUnit(this.value * other.value)
  def /(other: AreaUnit) = AreaUnit(this.value / other.value)
  def +(other: AreaUnit) = AreaUnit(this.value + other.value)
  def -(other: AreaUnit) = AreaUnit(this.value - other.value)
}

final case class LengthUnit(value: BigInt) extends Unit {
  def *(other: LengthUnit) = AreaUnit(this.value * other.value)
  def /(other: LengthUnit) = Unitless(this.value / other.value)
  def +(other: LengthUnit) = LengthUnit(this.value + other.value)
  def -(other: LengthUnit) = LengthUnit(this.value - other.value)

  def *(other: Unitless) = LengthUnit(this.value * other.value)
  def /(other: Unitless) = LengthUnit(this.value / other.value)
}

final case class AreaUnit(value: BigInt) extends Unit {
  def /(other: LengthUnit) = LengthUnit(this.value / other.value)

  def *(other: Unitless) = AreaUnit(this.value * other.value)
  def /(other: Unitless) = AreaUnit(this.value / other.value)

  def /(other: AreaUnit) = Unitless(this.value / other.value)
  def +(other: AreaUnit) = AreaUnit(this.value + other.value)
  def -(other: AreaUnit) = AreaUnit(this.value - other.value)
}

sealed trait Constraint[T <: Unit]

sealed abstract class Constrained[T <: Unit] extends Constraint[T]

sealed abstract class Unconstrained[T <: Unit] extends Constraint[T]
case object UnconstrainedUnitless extends Unconstrained[Unitless]
case object UnconstrainedLength extends Unconstrained[LengthUnit]
case object UnconstrainedArea extends Unconstrained[AreaUnit]

sealed abstract class ExactConstraint[T <: Unit] extends Constrained[T] {
  def value: T
}
final case class ExactUnitlessConstraint(value: Unitless) extends ExactConstraint[Unitless]
final case class ExactLengthConstraint(value: LengthUnit) extends ExactConstraint[LengthUnit]
final case class ExactAreaConstraint(value: AreaUnit) extends ExactConstraint[AreaUnit]

sealed abstract class GreaterThanConstraint[T <: Unit] extends Constrained[T] {
  def value: T
  def inclusive: Boolean
}
final case class GreaterThanUnitlessConstraint(value: Unitless, inclusive: Boolean) extends GreaterThanConstraint[Unitless]
final case class GreaterThanLengthConstraint(value: LengthUnit, inclusive: Boolean) extends GreaterThanConstraint[LengthUnit]
final case class GreaterThanAreaConstraint(value: AreaUnit, inclusive: Boolean) extends GreaterThanConstraint[AreaUnit]

sealed abstract class LessThanConstraint[T <: Unit] extends Constrained[T] {
  def value: T
  def inclusive: Boolean
}
final case class LessThanUnitlessConstraint(value: Unitless, inclusive: Boolean) extends LessThanConstraint[Unitless]
final case class LessThanLengthConstraint(value: LengthUnit, inclusive: Boolean) extends LessThanConstraint[LengthUnit]
final case class LessThanAreaConstraint(value: AreaUnit, inclusive: Boolean) extends LessThanConstraint[AreaUnit]

sealed abstract class MultipleOfConstraint[T <: Unit] extends Constrained[T] {
  def value: T
}
final case class MultipleOfUnitlessConstraint(value: Unitless) extends MultipleOfConstraint[Unitless]
final case class MultipleOfLengthConstraint(value: LengthUnit) extends MultipleOfConstraint[LengthUnit]
final case class MultipleOfAreaConstraint(value: AreaUnit) extends MultipleOfConstraint[AreaUnit]

// TODO do we have discrete combinations (e.g. BetweenConstraint) or do we provide a way to do unions?

sealed abstract class NotConstraint[T <: Unit] {
  def constraint: Constrained[T]
}
final case class NotUnitlessConstraint(constraint: Constrained[Unitless]) extends NotConstraint[Unitless]
final case class NotLengthConstraint(constraint: Constrained[LengthUnit]) extends NotConstraint[LengthUnit]
final case class NotAreaConstraint(constraint: Constrained[AreaUnit]) extends NotConstraint[AreaUnit]

sealed abstract class AndConstraint[T <: Unit] {
  def constraints: List[Constrained[T]]
}
final case class AndUnitlessConstraint(constraints: List[Constrained[Unitless]]) extends AndConstraint[Unitless]
final case class AndLengthConstraint(constraints: List[Constrained[LengthUnit]]) extends AndConstraint[LengthUnit]
final case class AndAreaConstraint(constraints: List[Constrained[AreaUnit]]) extends AndConstraint[AreaUnit]

sealed abstract class OrConstraint[T <: Unit] {
  def constraints: List[Constrained[T]]
}
final case class OrUnitlessConstraint(constraints: List[Constrained[Unitless]]) extends OrConstraint[Unitless]
final case class OrLengthConstraint(constraints: List[Constrained[LengthUnit]]) extends OrConstraint[LengthUnit]
final case class OrAreaConstraint(constraints: List[Constrained[AreaUnit]]) extends OrConstraint[AreaUnit]

