// See LICENSE for license details
package barstools.floorplan

private[floorplan] final case class IllegalUnitArithmeticException(a: HasUnit, b: HasUnit, op: String) extends Exception(s"Cannot ${op} ${a} and ${b}")

sealed trait HasUnit {

  def *(that: HasUnit): HasUnit
  def /(that: HasUnit): HasUnit
  def +(that: HasUnit): HasUnit
  def -(that: HasUnit): HasUnit
  def >(that: HasUnit): Boolean
  def <(that: HasUnit): Boolean
  def >=(that: HasUnit): Boolean
  def <=(that: HasUnit): Boolean
  def %(that: HasUnit): HasUnit
  def %?(that: HasUnit): Boolean
  def gcd(that: HasUnit): HasUnit
  def lcm(that: HasUnit): HasUnit

  def isPositive: Boolean

}

object Rational {

  def reduced(num: BigInt, denom: BigInt): Rational = {
    if (num == BigInt(0)) {
      Rational(BigInt(0), BigInt(1))
    } else {
      val gcd = num.gcd(denom)
      Rational(num/gcd, denom/gcd)
    }
  }

  def apply(ratio: Double, decimalPrecision: Long = 1000): Rational = {
    Rational.reduced(BigInt(scala.math.round(ratio*decimalPrecision)), BigInt(decimalPrecision))
  }

  def apply(l: Long): Rational = Rational(l, 1)

}

// TODO how to cleanly round when dividing?
final case class Rational(num: BigInt, denom: BigInt) extends HasUnit {

  assert(denom != 0, "Cannot divide by zero!")

  def invert = Rational(denom, num)
  def isZero = num == BigInt(0)
  def isPositive = (this.num*this.denom) > BigInt(0)

  private def lcm(a: BigInt, b: BigInt) = (a*b)/a.gcd(b)

  def *(that: HasUnit) = that match {
    case that: Rational   => Rational.reduced(this.num * that.num, this.denom * that.denom)
    case that: LengthUnit => LengthUnit(this.num * that.value / this.denom)
    case that: AreaUnit   => AreaUnit(this.num * that.value / this.denom)
    case _ => throw new IllegalUnitArithmeticException(this, that, "*")
  }

  def /(that: HasUnit) = that match {
    case that: Rational   => Rational.reduced(this.num * that.denom, this.denom * that.num)
    case _ => throw new IllegalUnitArithmeticException(this, that, "/")
  }

  def +(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      val newNum = (this.num * (newDenom / this.denom)) + (that.num * (newDenom / that.denom))
      Rational.reduced(newNum, newDenom)
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "+")
  }

  def -(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      val newNum = (this.num * (newDenom / this.denom)) - (that.num * (newDenom / that.denom))
      Rational.reduced(newNum, newDenom)
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "-")
  }

  def >(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      (this.num * (newDenom / this.denom)) > (that.num * (newDenom / that.denom))
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, ">")
  }

  def <(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      (this.num * (newDenom / this.denom)) < (that.num * (newDenom / that.denom))
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "<")
  }

  def >=(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      (this.num * (newDenom / this.denom)) >= (that.num * (newDenom / that.denom))
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, ">=")
  }

  def <=(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      (this.num * (newDenom / this.denom)) <= (that.num * (newDenom / that.denom))
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "<=")
  }

  def %(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      val newNum = (this.num * (newDenom / this.denom)) % (that.num * (newDenom / that.denom))
      Rational.reduced(newNum, newDenom)
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "%")
  }

  def %?(that: HasUnit) = that match {
    case that: Rational => (this % that).asInstanceOf[Rational].isZero
    case _ => throw new IllegalUnitArithmeticException(this, that, "%?")
  }

  def gcd(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      val newNum = (this.num * (newDenom / this.denom)).gcd(that.num * (newDenom / that.denom))
      Rational.reduced(newNum, newDenom)
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "gcd")
  }

  def lcm(that: HasUnit) = that match {
    case that: Rational => {
      val newDenom = lcm(this.denom, that.denom)
      val newNum = lcm(this.num * (newDenom / this.denom), that.num * (newDenom / that.denom))
      Rational.reduced(newNum, newDenom)
    }
    case _ => throw new IllegalUnitArithmeticException(this, that, "lcm")
  }

}

final case class LengthUnit private[floorplan] (value: BigInt) extends HasUnit {

  def isPositive = this.value > BigInt(0)

  def *(that: HasUnit) = that match {
    case that: Rational   => LengthUnit((this.value * that.num) / that.denom)
    case that: LengthUnit => AreaUnit(this.value * that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "*")
  }

  def /(that: HasUnit) = that match {
    case that: Rational   => LengthUnit((this.value * that.denom) / that.num)
    case that: LengthUnit => Rational.reduced(this.value, that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "/")
  }

  def +(that: HasUnit) = that match {
    case that: LengthUnit => LengthUnit(this.value + that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "+")
  }

  def -(that: HasUnit) = that match {
    case that: LengthUnit => LengthUnit(this.value - that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "-")
  }

  def >(that: HasUnit) = that match {
    case that: LengthUnit => (this.value > that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, ">")
  }

  def <(that: HasUnit) = that match {
    case that: LengthUnit => (this.value < that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "<")
  }

  def >=(that: HasUnit) = that match {
    case that: LengthUnit => (this.value >= that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, ">=")
  }

  def <=(that: HasUnit) = that match {
    case that: LengthUnit => (this.value <= that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "<=")
  }

  def %(that: HasUnit) = that match {
    case that: LengthUnit => LengthUnit(this.value % that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "%")
  }

  def %?(that: HasUnit) = that match {
    case that: LengthUnit => (this % that).asInstanceOf[LengthUnit].value == BigInt(0)
    case _ => throw new IllegalUnitArithmeticException(this, that, "%")
  }

  def gcd(that: HasUnit) = that match {
    case that: LengthUnit => LengthUnit(this.value.gcd(that.value))
    case _ => throw new IllegalUnitArithmeticException(this, that, "gcd")
  }

  def lcm(that: HasUnit) = that match {
    case that: LengthUnit => LengthUnit(this.value*that.value/this.value.gcd(that.value))
    case _ => throw new IllegalUnitArithmeticException(this, that, "lcm")
  }

}

final case class AreaUnit private[floorplan] (value: BigInt) extends HasUnit {

  def isPositive = this.value > BigInt(0)

  def *(that: HasUnit) = that match {
    case that: Rational => AreaUnit((this.value * that.num) / that.denom)
    case _ => throw new IllegalUnitArithmeticException(this, that, "*")
  }

  def /(that: HasUnit) = that match {
    case that: Rational => AreaUnit((this.value * that.denom) / that.num)
    case that: LengthUnit => LengthUnit(this.value / that.value)
    case that: AreaUnit => Rational.reduced(this.value, that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "/")
  }

  def +(that: HasUnit) = that match {
    case that: AreaUnit => AreaUnit(this.value + that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "+")
  }

  def -(that: HasUnit) = that match {
    case that: AreaUnit => AreaUnit(this.value - that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "-")
  }

  def >(that: HasUnit) = that match {
    case that: AreaUnit => (this.value > that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, ">")
  }

  def <(that: HasUnit) = that match {
    case that: AreaUnit => (this.value < that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "<")
  }

  def >=(that: HasUnit) = that match {
    case that: AreaUnit => (this.value >= that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, ">=")
  }

  def <=(that: HasUnit) = that match {
    case that: AreaUnit => (this.value <= that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "<=")
  }

  def %(that: HasUnit) = that match {
    case that: AreaUnit => AreaUnit(this.value % that.value)
    case _ => throw new IllegalUnitArithmeticException(this, that, "%")
  }

  def %?(that: HasUnit) = that match {
    case that: AreaUnit => (this % that).asInstanceOf[AreaUnit].value == BigInt(0)
    case _ => throw new IllegalUnitArithmeticException(this, that, "%")
  }

  def gcd(that: HasUnit) = that match {
    case that: AreaUnit => AreaUnit(this.value.gcd(that.value))
    case _ => throw new IllegalUnitArithmeticException(this, that, "gcd")
  }

  def lcm(that: HasUnit) = that match {
    case that: AreaUnit => AreaUnit(this.value*that.value/this.value.gcd(that.value))
    case _ => throw new IllegalUnitArithmeticException(this, that, "lcm")
  }

}

sealed trait Constraint[T <: HasUnit] {

  def and(that: Constraint[T]): Constraint[T]

}

sealed abstract class PrimitiveConstraint[T <: HasUnit] extends Constraint[T]

final class Unconstrained[T <: HasUnit] extends PrimitiveConstraint[T] {

  def and(that: Constraint[T]) = that

}

object Unconstrained {

  def apply[T <: HasUnit] = new Unconstrained[T]

}

final case class EqualTo[T <: HasUnit](unit: T) extends PrimitiveConstraint[T] {

  assert(unit.isPositive, "EqualTo value must be postiive")

  def and(that: Constraint[T]) = that match {
    case that: EqualTo[T]              => if (this.unit == that.unit) this else ImpossibleConstraint(this, that)
    case that: GreaterThanOrEqualTo[T] => if (this.unit >= that.unit) this else ImpossibleConstraint(this, that)
    case that: LessThanOrEqualTo[T]    => if (this.unit <= that.unit) this else ImpossibleConstraint(this, that)
    case that: MultipleOf[T]           => if (this.unit %? that.unit) this else ImpossibleConstraint(this, that)
    case that: ImpossibleConstraint[T] => that
    case that: Unconstrained[T]        => this
    case that => AndConstraint(this, that)
  }

}

final case class GreaterThanOrEqualTo[T <: HasUnit](unit: T) extends PrimitiveConstraint[T] {

  assert(unit.isPositive , "GreaterThanOrEqualTo value must be postiive")

  def and(that: Constraint[T]) = that match {
    case that: EqualTo[T]              => if (this.unit <= that.unit) that else ImpossibleConstraint(this, that)
    case that: GreaterThanOrEqualTo[T] => if (this.unit >= that.unit) this else that
    case that: LessThanOrEqualTo[T]    => if (this.unit < that.unit) AndConstraint(this, that) else
      if (this.unit == that.unit) EqualTo(this.unit) else ImpossibleConstraint(this, that)
    case that: ImpossibleConstraint[T] => that
    case that: Unconstrained[T]        => this
    case that => AndConstraint(this, that)
  }

}

final case class LessThanOrEqualTo[T <: HasUnit](unit: T) extends PrimitiveConstraint[T] {

  assert(unit.isPositive, "LessThanOrEqualTo value must be positive")

  def and(that: Constraint[T]) = that match {
    case that: EqualTo[T]              => if (this.unit >= that.unit) that else ImpossibleConstraint(this, that)
    case that: GreaterThanOrEqualTo[T] => if (this.unit > that.unit) AndConstraint(this, that) else
      if (this.unit == that.unit) EqualTo(this.unit) else ImpossibleConstraint(this, that)
    case that: LessThanOrEqualTo[T]    => if (this.unit <= that.unit) this else that
    case that: ImpossibleConstraint[T] => that
    case that: Unconstrained[T]        => this
    case that => AndConstraint(this, that)
  }

}

// TODO allow offset
final case class MultipleOf[T <: HasUnit](unit: T) extends PrimitiveConstraint[T] {

  assert(unit.isPositive, "MultipleOf value must be positive")

  def and(that: Constraint[T]) = that match {
    case that: EqualTo[T]              => if (that.unit %? this.unit) that else ImpossibleConstraint(this, that)
    case that: MultipleOf[T]           => MultipleOf((this.unit lcm that.unit).asInstanceOf[T])
    case that: LessThanOrEqualTo[T]    => if (that.unit < this.unit) ImpossibleConstraint(this, that) else AndConstraint(this, that)
    case that: ImpossibleConstraint[T] => that
    case that: Unconstrained[T]        => this
    case that => AndConstraint(this, that)
  }

}

sealed abstract class AggregateConstraint[T <: HasUnit] extends Constraint[T]

final case class ImpossibleConstraint[T <: HasUnit](a: Constraint[T], b: Constraint[T]) extends AggregateConstraint[T] {

  def and(that: Constraint[T]) = this

}

final case class AndConstraint[T <: HasUnit](constraints: Seq[Constraint[T]]) extends AggregateConstraint[T] {

  def and(that: Constraint[T]) = that match {
    case that: ImpossibleConstraint[T] => that
    case that: Unconstrained[T]        => this
    case that => AndConstraint(this, that)
  }

  def flatten: AndConstraint[T] = {
    AndConstraint(constraints.collect({
      case x: AndConstraint[T] => x.flatten.constraints
      case x: PrimitiveConstraint[T] => Seq(x)
    }).reduce(_ ++ _))
  }

  def reduce: Constraint[T] = {
    val flat = this.flatten.constraints

    val exact = flat.collect({
      case x:EqualTo[T] => x
    }).reduceOption[Constraint[T]](_.and(_))

    val gt = flat.collect({
      case x:GreaterThanOrEqualTo[T] => x
    }).reduceOption[Constraint[T]](_.and(_))

    val lt = flat.collect({
      case x:LessThanOrEqualTo[T] => x
    }).reduceOption[Constraint[T]](_.and(_))

    val mult = flat.collect({
      case x:MultipleOf[T] => x
    }).reduceOption[Constraint[T]](_.and(_))

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

  def apply[T <: HasUnit](a: Constraint[T], b: Constraint[T]): AndConstraint[T] = AndConstraint(Seq(a, b))

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

final case class RatioPlacementConstraint(x: Constraint[Rational], y: Constraint[Rational], anchor: PlacementAnchor = LowerLeft())
final case class LengthPlacementConstraint(x: Constraint[LengthUnit], y: Constraint[LengthUnit], anchor: PlacementAnchor = LowerLeft())

