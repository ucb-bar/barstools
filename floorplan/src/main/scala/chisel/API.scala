// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule}

import firrtl.annotations.{InstanceTarget}

import barstools.floorplan._
import barstools.floorplan.firrtl.{FloorplanAnnotation, FloorplanInstanceAnnotation, FloorplanModuleAnnotation}
import scala.collection.mutable.{ArraySeq, HashMap, Set, HashSet}

final case class ChiselFloorplanException(message: String) extends Exception(message: String)

object Floorplan {

  def createRect[T <: RawModule](module: T,
    width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
    aspectRatio: Constraint[Rational] = Unconstrained[Rational],
    hardBoundary: Boolean = true
  ) = {
    val elt = new ChiselLogicRect(module, width, height, area, aspectRatio, hardBoundary)
    FloorplanDatabase.register(module, elt)
    elt
  }

  def createPlaceholderRect[T <: RawModule](module: T,
    name: Option[String] = None,
    width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
    aspectRatio: Constraint[Rational] = Unconstrained[Rational]
  ) = {
    val nameStr = name.getOrElse(FloorplanDatabase.getUnusedName(module))
    val elt = new ChiselPlaceholderRect(module, nameStr, width, height, area, aspectRatio)
    FloorplanDatabase.register(module, elt)
    elt
  }

  def createGrid[T <: RawModule](module: T,
    name: String,
    x: Int = 1,
    y: Int = 1,
    packed: Boolean = false
  ) = {
    val elt = new ChiselWeightedGrid(name, module, x, y, packed)
    FloorplanDatabase.register(module, elt)
    elt
  }

  def commitAndGetAnnotations(): Seq[FloorplanAnnotation] = FloorplanDatabase.commitAndGetAnnotations()

}

private[chisel] object FloorplanDatabase {

  private val nameMap = new HashMap[RawModule, Set[String]]()
  private val elements = new HashSet[ChiselElement]()

  private def getSet(module: RawModule) = nameMap.getOrElseUpdate(module, new HashSet[String])

  def register(module: RawModule, element: ChiselElement): Unit = {
    val name = element.name
    val set = getSet(module)
    if (set.contains(name)) {
      throw new ChiselFloorplanException(s"Duplicate floorplan element registration ${name} for module ${module.getClass.getName}!")
    }
    elements.add(element)
    set.add(name)
  }

  def getUnusedName(module: RawModule, suggestion: String = "unnamed"): String = {
    val set = getSet(module)
    var id = 0
    while (set.contains(suggestion + s"_${id}")) { id = id + 1 }
    suggestion + s"_${id}"
  }

  def commitAndGetAnnotations(): Seq[FloorplanAnnotation] = {
    elements.foreach(_.commit())
    elements.toSeq.map(_.getAnnotation())
  }

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

abstract class ChiselElement(val module: RawModule, val name: String) {

  final private var committed = false

  protected def generateElement(): Element

  // TODO FIXME this is clunky
  final protected def targetName: (String, String) = (s"${module.toAbsoluteTarget.serialize}", name)

  // TODO FIXME this is clunky too
  final private var fpir: Element = null

  def getAnnotation(): FloorplanAnnotation = {
    if (!committed) throw ChiselFloorplanException("Must commit ChiselElement before getting its annotation!")
    FloorplanInstanceAnnotation(module.toAbsoluteTarget.asInstanceOf[InstanceTarget], fpir)
  }

  final private[chisel] def commit(): (String, String) = {
    if (!committed) {
      committed = true
      fpir = generateElement()
    }
    targetName
  }

}

final class ChiselLogicRect private[chisel] (
  module: RawModule,
  width: Constraint[LengthUnit],
  height: Constraint[LengthUnit],
  area: Constraint[AreaUnit],
  aspectRatio: Constraint[Rational],
  hardBoundary: Boolean
) extends ChiselElement(module, "") {

  protected def generateElement(): Element = ConstrainedLogicRect(width, height, area, aspectRatio, hardBoundary)

}

final class ChiselPlaceholderRect private[chisel] (
  module: RawModule,
  name: String,
  width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ChiselElement(module, name) {

  protected def generateElement(): Element = ConstrainedPlaceholderRect(name, width, height, area, aspectRatio)

}

final class ChiselWeightedGrid private[chisel] (
  name: String,
  module: RawModule,
  x: Int,
  y: Int,
  packed: Boolean
) extends ChiselElement(module, name) {

  assert(x > 0)
  assert(y > 0)

  // TODO add resizing APIs
  private var xDim = x
  private var yDim = y

  private val elements = ArraySeq.fill[Option[ChiselElement]](xDim, yDim) { Option.empty[ChiselElement] }
  private val weights = ArraySeq.fill[Rational](xDim, yDim) { Rational(1) }

  def set(x: Int, y: Int, element: ChiselElement, weight: Rational = Rational(1)): Unit = {
    if (x >= xDim) throw new IndexOutOfBoundsException(s"X-value ${x} >= ${xDim} in ChiselWeightedGrid")
    if (y >= yDim) throw new IndexOutOfBoundsException(s"Y-value ${y} >= ${yDim} in ChiselWeightedGrid")
    elements(x)(y) = Some(element)
    weights(x)(y) = weight
  }

  def setModule(x: Int, y: Int, elementModule: RawModule, weight: Rational = Rational(1)): Unit = set(x, y, Floorplan.createRect(elementModule), weight)

  protected def generateElement(): Element = WeightedGrid(
    name,
    xDim,
    yDim,
    elements.flatten.map(_.getOrElse(Floorplan.createPlaceholderRect(module)).commit()),
    weights.flatten,
    packed)

}
