// See LICENSE for license details
package barstools.floorplan

import scala.math.{BigInt, BigDecimal}

sealed trait Constraint {
  def and(that: Constraint): Constraint
}

sealed abstract class PrimitiveConstraint extends Constraint

final class Unconstrained extends PrimitiveConstraint {
  def and(that: Constraint) = that
}

object Unconstrained {
  private val u = new Unconstrained
  def apply() = u
}

final case class EqualTo(value: BigDecimal) extends PrimitiveConstraint {
  def and(that: Constraint) = that match {
    case that: EqualTo              => if (this.value == that.value) this else ImpossibleConstraint(this, that)
    case that: GreaterThanOrEqualTo => if (this.value >= that.value) this else ImpossibleConstraint(this, that)
    case that: LessThanOrEqualTo    => if (this.value <= that.value) this else ImpossibleConstraint(this, that)
    case that: MultipleOf           => if ((this.value % that.value) == BigDecimal(0)) this else ImpossibleConstraint(this, that)
    case that: ImpossibleConstraint => that
    case that: Unconstrained        => this
    case that => AndConstraint(this, that)
  }
}

final case class GreaterThanOrEqualTo(value: BigDecimal) extends PrimitiveConstraint {
  def and(that: Constraint) = that match {
    case that: EqualTo              => if (this.value <= that.value) that else ImpossibleConstraint(this, that)
    case that: GreaterThanOrEqualTo => if (this.value >= that.value) this else that
    case that: LessThanOrEqualTo    => if (this.value < that.value) AndConstraint(this, that) else
      if (this.value == that.value) EqualTo(this.value) else ImpossibleConstraint(this, that)
    case that: ImpossibleConstraint => that
    case that: Unconstrained        => this
    case that => AndConstraint(this, that)
  }
}

final case class LessThanOrEqualTo(value: BigDecimal) extends PrimitiveConstraint {
  def and(that: Constraint) = that match {
    case that: EqualTo              => if (this.value >= that.value) that else ImpossibleConstraint(this, that)
    case that: GreaterThanOrEqualTo => if (this.value > that.value) AndConstraint(this, that) else
      if (this.value == that.value) EqualTo(this.value) else ImpossibleConstraint(this, that)
    case that: LessThanOrEqualTo    => if (this.value <= that.value) this else that
    case that: ImpossibleConstraint => that
    case that: Unconstrained        => this
    case that => AndConstraint(this, that)
  }
}

// TODO allow offset
final case class MultipleOf(value: BigDecimal) extends PrimitiveConstraint {
  def and(that: Constraint) = that match {
    case that: EqualTo              => if ((that.value % this.value) == BigDecimal(0)) that else ImpossibleConstraint(this, that)
    case that: MultipleOf           => MultipleOf(lcm(this.value,that.value))
    case that: LessThanOrEqualTo    => if (that.value < this.value) ImpossibleConstraint(this, that) else AndConstraint(this, that)
    case that: ImpossibleConstraint => that
    case that: Unconstrained        => this
    case that => AndConstraint(this, that)
  }
}

sealed abstract class AggregateConstraint extends Constraint

final case class ImpossibleConstraint(a: Constraint, b: Constraint) extends AggregateConstraint {
  def and(that: Constraint) = this
}

final case class AndConstraint(constraints: Seq[Constraint]) extends AggregateConstraint {
  def and(that: Constraint) = that match {
    case that: ImpossibleConstraint => that
    case that: Unconstrained        => this
    case that => AndConstraint(this, that)
  }

  def flatten: AndConstraint = {
    AndConstraint(constraints.collect({
      case x: AndConstraint => x.flatten.constraints
      case x: PrimitiveConstraint => Seq(x)
    }).reduce(_ ++ _))
  }

  def reduce: Constraint = {
    val flat = this.flatten.constraints

    val exact = flat.collect({
      case x:EqualTo => x
    }).reduceOption[Constraint](_.and(_))

    val gt = flat.collect({
      case x:GreaterThanOrEqualTo => x
    }).reduceOption[Constraint](_.and(_))

    val lt = flat.collect({
      case x:LessThanOrEqualTo => x
    }).reduceOption[Constraint](_.and(_))

    val mult = flat.collect({
      case x:MultipleOf => x
    }).reduceOption[Constraint](_.and(_))

    // if exact is defined, we'll either be exact or impossible
    exact.map({ value =>
      val postGt = gt.map(_.and(value)).getOrElse(value)
      val postLt = lt.map(_.and(postGt)).getOrElse(postGt)
      mult.map(_.and(postLt)).getOrElse(postLt)
    }).getOrElse {
      ???
    }
  }
}

object AndConstraint {
  def apply(a: Constraint, b: Constraint): AndConstraint = AndConstraint(Seq(a, b))
}

sealed trait PlacementAnchor

class LowerLeft extends PlacementAnchor
class LowerMiddle extends PlacementAnchor
class LowerRight extends PlacementAnchor
class CenterLeft extends PlacementAnchor
class CenterMiddle extends PlacementAnchor
class CenterRight extends PlacementAnchor
class UpperLeft extends PlacementAnchor
class UpperMiddle extends PlacementAnchor
class UpperRight extends PlacementAnchor

object LowerLeft { def apply() = new LowerLeft }
object LowerMiddle { def apply() = new LowerMiddle }
object LowerRight { def apply() = new LowerRight }
object CenterLeft { def apply() = new CenterLeft }
object CenterMiddle { def apply() = new CenterMiddle }
object CenterRight { def apply() = new CenterRight }
object UpperLeft { def apply() = new UpperLeft }
object UpperMiddle { def apply() = new UpperMiddle }
object UpperRight { def apply() = new UpperRight }

