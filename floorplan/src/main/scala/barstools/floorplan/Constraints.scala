// See LICENSE for license details
package barstools.floorplan

import scala.math.{BigInt, BigDecimal}

sealed trait Constraint {
  def and(that: Constraint): Constraint
  def +(that: Constraint): Constraint
}

final class Unconstrained extends Constraint {
  def and(that: Constraint) = that
  def +(that: Constraint) = that // ???
}

object Unconstrained {
  private val u = new Unconstrained
  def apply() = u
}

object UnconstrainedSeq {
  def apply(x: Int) = Seq.fill(x) { Unconstrained() }
}

// TODO add a reason?
final class Impossible extends Constraint {
  def and(that: Constraint) = this
  def +(that: Constraint) = this
}

object Impossible {
  private val i = new Impossible
  def apply() = i
}

final case class Constrained(
  eq: Option[BigDecimal],
  geq: Option[BigDecimal],
  leq: Option[BigDecimal],
  mof: Option[BigDecimal]
) extends Constraint {

  def and(that: Constraint): Constraint = {
    that match {
      case that: Unconstrained => this
      case that: Impossible => that
      case that: Constrained =>

        // Combine raw constraints
        val newMof = if (this.mof.isDefined && that.mof.isDefined) {
          Some(lcm(this.mof.get, that.mof.get))
        } else {
          this.mof.orElse(that.mof)
        }

        val newLeq = if (this.leq.isDefined && that.leq.isDefined) {
          Some(Seq(this.leq.get, that.leq.get).min)
        } else {
          this.leq.orElse(that.leq)
        }

        val newGeq = if (this.geq.isDefined && that.geq.isDefined) {
          Some(Seq(this.geq.get, that.geq.get).max)
        } else {
          this.geq.orElse(that.geq)
        }

        if (this.eq.isDefined && that.eq.isDefined && (this.eq != that.eq)) {
          return Impossible()
        }
        val newEq = this.eq.orElse(that.eq)

        Constrained(eq=newEq, geq=newGeq, leq=newLeq, mof=newMof).minimize
      case _ => ???
    }
  }

  def minimize: Constraint = {
    // Check range on LEQ/GEQ
    val newEq = if (leq.isDefined && geq.isDefined) {
      if (leq.get < geq.get) {
        return Impossible()
      } else if (leq.get == geq.get) {
        if (eq.isDefined && (eq.get != leq.get)) {
          return Impossible()
        }
        leq
      } else {
        eq
      }
    } else {
      eq
    }

    // Check multiples
    if (eq.isDefined && mof.isDefined && (eq.get % mof.get != BigDecimal(0))) {
      return Impossible()
    }

    if (eq.isDefined) {
      if (leq.isDefined) {
        if (eq.get > leq.get) {
          return Impossible()
        }
      }
      if (geq.isDefined) {
        if (eq.get < geq.get) {
          return Impossible()
        }
      }
    }
    // TODO check if there exists a multiple in range
    this.copy(eq=newEq)
  }

  def +(that: Constraint): Constraint = {
    that match {
      case that: Unconstrained => this
      case that: Impossible => that
      case that: Constrained =>

        // Combine raw constraints
        val newMof = if (this.mof == that.mof) {
          this.mof
        } else {
          None
        }

        val newLeq = if (this.leq.isDefined && that.leq.isDefined) {
          Some(this.leq.get + that.leq.get)
        } else {
          None
        }

        val newGeq = if (this.geq.isDefined && that.geq.isDefined) {
          Some(this.geq.get + that.geq.get)
        } else {
          None
        }

        val newEq = if (this.eq.isDefined && that.eq.isDefined) {
          Some(this.eq.get + that.eq.get)
        } else {
          None
        }

        Constrained(eq=newEq, geq=newGeq, leq=newLeq, mof=newMof).minimize
      case _ => ???
    }
  }
}

object EqualTo {
  def apply(value: BigDecimal) = Constrained(Some(value), None, None, None)
}

object GreaterThanOrEqualTo {
  def apply(value: BigDecimal) = Constrained(None, Some(value), None, None)
}

object LessThanOrEqualTo {
  def apply(value: BigDecimal) = Constrained(None, None, Some(value), None)
}

object MultipleOf {
  def apply(value: BigDecimal) = Constrained(None, None, None, Some(value))
}

case class Constraints(
  width: Constraint = Unconstrained(),
  height: Constraint = Unconstrained(),
  area: Constraint = Unconstrained(),
  aspectRatio: Constraint = Unconstrained()
)

object Constraints {

  def sized(w: BigDecimal, h: BigDecimal): Constraints = {
    Constraints(
      EqualTo(w),
      EqualTo(h),
      Unconstrained(),
      Unconstrained()
    )
  }
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

