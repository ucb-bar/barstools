// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule}

import barstools.floorplan._

object Floorplan {

  def createRect[T <: RawModule](m: T,
    width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
    aspectRatio: Constraint[Rational] = Unconstrained[Rational]) = ???

}

object FloorplanUnits {

  // TODO do we make this an annotation?
  private final case class FloorplanUnitsException(message: String) extends Exception(message)

  // This corresponds to the scaling factor between user input and database units, such that the
  // smallest database unit will correspond to 1. Currently only supports units <= 1.
  // e.g.
  //
  // db unit  |  unit
  // 0.001 um |  1000
  // 0.005 um |   200
  // 1 um     |     1
  // 10 um    | error
  private var unit = Option.empty[Long]

  def set(x: Double) {
    if (x > 1) {
      throw new FloorplanUnitsException("Cannot set base unit to a value > 1.")
    }
    if (unit.nonEmpty) {
      throw new FloorplanUnitsException("Cannot set units more than once!")
    }
    unit = Some(scala.math.round(1/x))
  }

  def get = unit

  def area(x: Double): AreaUnit = {
    unit.map { u =>
      AreaUnit(scala.math.round(u*x*x))
    }.getOrElse {
      throw new FloorplanUnitsException("Cannot create floorplan with concrete units- units have not been set! Call FloorplanUnits.set first.")
    }
  }

  def length(x: Double): LengthUnit = {
    unit.map { u =>
      LengthUnit(scala.math.round(u*x))
    }.getOrElse {
      throw new FloorplanUnitsException("Cannot create floorplan with concrete units- units have not been set! Call FloorplanUnits.set first.")
    }
  }


}

