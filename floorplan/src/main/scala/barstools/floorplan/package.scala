// See LICENSE for license details
package barstools
package object floorplan {

import scala.math.{BigInt, BigDecimal}
import scala.collection.Seq

def gcd(a: BigDecimal, b: BigDecimal): BigDecimal = {
  val scaleFactor = 1 << Seq(a.scale, b.scale).max
  val scaledGCD = (a*scaleFactor).toBigInt.gcd((b*scaleFactor).toBigInt)
  BigDecimal(scaledGCD, scaleFactor)
}

def lcm(a: BigDecimal, b: BigDecimal): BigDecimal = (a*b)/gcd(a,b)

type PrimitiveConstraint = Constraint

}
