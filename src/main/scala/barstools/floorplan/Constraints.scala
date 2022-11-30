// See LICENSE for license details
package barstools.floorplan

import scala.math.{BigInt, BigDecimal, sqrt}

sealed trait Constraint {
  def and(that: Constraint): Constraint
  def +(that: Constraint): Constraint
  def *(that: BigDecimal): Constraint
  def /(that: BigDecimal): Constraint
  def test(value: BigDecimal): Boolean
  def minimize: Constraint

  def resolveMin: BigDecimal = this.minimize match {
    case c: Impossible => throw new Exception("Cannot reduce impossible constraint. TODO provide more detailed debug info.")
    case c: Unconstrained => BigDecimal(0)
    case c: Constrained =>
      c.eq.foreach { x => return x }
      c.geq.foreach { x =>
        c.mof.foreach { m =>
          val n = (x/m).setScale(0, BigDecimal.RoundingMode.UP)
          m*n
        }
        return x
      }
      BigDecimal(0)
  }
  def getConstraint: BigDecimal = this.minimize match {
      case c: Impossible => throw new Exception("This is not constrined")
      case c: Unconstrained => BigDecimal(0)
      case c: Constrained =>
        c.eq.foreach {x => return x }
        c.geq.foreach { x =>
            c.mof.foreach { m =>
            val n = (x/m).setScale(0, BigDecimal.RoundingMode.UP)
            m*n
            }
            return x
        }
        c.leq.foreach {x => return x}
        BigDecimal(0)
  }

  def isConstrained: Boolean
}

final class Unconstrained extends Constraint {
  def and(that: Constraint) = that
  def +(that: Constraint) = that // ???
  def *(that: BigDecimal) = this
  def /(that: BigDecimal) = this
  def test(value: BigDecimal) = true
  def minimize = this
  def isConstrained = false
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
  def *(that: BigDecimal) = this
  def /(that: BigDecimal) = this
  def test(value: BigDecimal) = false
  def minimize = this
  def isConstrained = true
}

object Impossible {
  private val i = new Impossible
  def apply() = i
}

final case class Constrained(
  eq: Option[BigDecimal] = None,
  geq: Option[BigDecimal] = None,
  leq: Option[BigDecimal] = None,
  mof: Option[BigDecimal] = None
) extends Constraint {

  def isConstrained: Boolean = eq.isDefined || geq.isDefined || leq.isDefined || mof.isDefined

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

        val newLeq = if (this.leq.isDefined && that.eq.isDefined) {
          Some(this.leq.get + that.eq.get)
        } else if (this.eq.isDefined && that.leq.isDefined) {
          Some(this.eq.get + that.leq.get)
        } else if (this.leq.isDefined && that.leq.isDefined) {
          Some(this.leq.get + that.leq.get)
        } else {
          None
        }

        val newGeq = if (this.geq.isDefined && that.eq.isDefined) {
          Some(this.geq.get + that.eq.get)
        } else if (this.eq.isDefined && that.geq.isDefined) {
          Some(this.eq.get + that.geq.get)
        } else if (this.geq.isDefined && that.geq.isDefined) {
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

  def *(that: BigDecimal): Constraint = Constrained(
    eq = this.eq.map(_ * that),
    geq = this.geq.map(_ * that),
    leq = this.leq.map(_ * that),
    mof = this.mof.map(_ * that)
  )

  def /(that: BigDecimal): Constraint = Constrained(
    eq = this.eq.map(_ / that),
    geq = this.geq.map(_ / that),
    leq = this.leq.map(_ / that),
    mof = this.mof.map(_ / that)
  )

  def test(value: BigDecimal): Boolean = {
    val eqTest = this.eq.map(_ == value).getOrElse(true)
    val geqTest = this.geq.map(_ <= value).getOrElse(true)
    val leqTest = this.leq.map(_ >= value).getOrElse(true)
    val mofTest = this.mof.map(x => (value % x) == 0).getOrElse(true)
    return eqTest && geqTest && leqTest && mofTest
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
) {
  def test(widthValue: BigDecimal, heightValue: BigDecimal): Boolean = {
    val widthTest = width.test(widthValue)
    val heightTest = height.test(heightValue)
    val areaTest = area.test(widthValue*heightValue)
    val arTest = aspectRatio.test(heightValue/widthValue)
    widthTest && heightTest && areaTest && arTest
  }

  def weightXY(xWeight: BigDecimal, yWeight: BigDecimal): Constraints = Constraints(
    width = this.width * xWeight,
    height = this.height * yWeight,
    area = this.area * (xWeight * yWeight),
    aspectRatio = this.aspectRatio * (yWeight / xWeight)
  )

  def resolveMinDimensions(): (BigDecimal, BigDecimal) = resolveMinDimensions(BigDecimal(0), BigDecimal(0))

  def resolveMinDimensions(defaultWidth: BigDecimal, defaultHeight: BigDecimal): (BigDecimal, BigDecimal) = {
    if (this.aspectRatio.isConstrained) {
      if (this.area.isConstrained) {
        // AspectRatio with Area
        if (this.width == Unconstrained() && this.height == Unconstrained()) {
          val heightConstraint = BigDecimal(sqrt((this.area.resolveMin * this.aspectRatio.resolveMin).doubleValue)) // TODO clean up rounding
          (this.area.resolveMin / heightConstraint, heightConstraint)
        } else {
          // TODO resolve 3- or 4-way constraint (this is wrong)
          ???
        }
      } else {
        // AspectRatio with no Area
        // Use defaultWidth (TODO make this an option?)
        if (this.width == Unconstrained() && this.height == Unconstrained()) {
          (defaultWidth, this.aspectRatio.resolveMin * defaultWidth)
        } else if (this.height == Unconstrained()) {
          (this.height.resolveMin / this.aspectRatio.resolveMin, this.height.resolveMin)
        } else if (this.width == Unconstrained()) {
          (this.width.resolveMin, this.aspectRatio.resolveMin * this.width.resolveMin)
        } else {
          // TODO resolve 3-way constraint
          ???
        }
      }
    } else {
      if (this.area.isConstrained) {
        // Area with no AspectRatio
        // Use defaultWidth (TODO make this an option?)
        if (this.width == Unconstrained() && this.height == Unconstrained()) {
          (defaultWidth, this.area.resolveMin / defaultWidth)
        } else if (this.height == Unconstrained()) {
          (this.width.resolveMin, this.area.resolveMin / this.width.resolveMin)
        } else if (this.width == Unconstrained()) {
          (this.area.resolveMin / this.height.resolveMin, this.height.resolveMin)
        } else {
          // TODO resolve 3-way constraint (this is wrong)
          val widthReq = Seq(this.width.resolveMin, this.area.resolveMin / this.height.resolveMin).max
          val heightReq = Seq(this.width.resolveMin, this.area.resolveMin / this.width.resolveMin).max
          (widthReq, heightReq)
        }
      } else {
        // No Area or AspectRatio
        val widthConstraint = this.width match {
          case x: Unconstrained => defaultWidth
          case x => x.resolveMin
        }
        val heightConstraint = this.height match {
          case x: Unconstrained => defaultHeight
          case x => x.resolveMin
        }
        (widthConstraint, heightConstraint)
      }
    }
  }

}

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

// TODO use this and convert to Enum
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

