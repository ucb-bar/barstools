// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule}

import firrtl.annotations.{ModuleTarget, InstanceTarget}

import barstools.floorplan._
import barstools.floorplan.firrtl.{FloorplanAnnotation, FloorplanInstanceAnnotation, FloorplanModuleAnnotation, FloorplanGroupAnnotation}
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
    val elt = new ChiselWeightedGrid(module, name, x, y, packed)
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
  //final protected def targetName: (String, String) = (s"${module.toAbsoluteTarget.serialize}", name)

  // TODO FIXME this is clunky too
  final protected var fpir: Element = null

  def getAnnotation(): FloorplanAnnotation

  def isCommitted = committed

  final private[chisel] def commit(): String = {
    if (!isCommitted) {
      committed = true
      fpir = generateElement()
    }
    name
  }

}

abstract class ChiselPrimitiveElement(module: RawModule, name: String) extends ChiselElement(module, name) {

  def getAnnotation(): FloorplanAnnotation = {
    if (!isCommitted) throw ChiselFloorplanException("Must commit ChiselElement before getting its annotation!")
    module.toAbsoluteTarget match {
      case x: InstanceTarget => FloorplanInstanceAnnotation(x, fpir)
      case x: ModuleTarget => FloorplanModuleAnnotation(x, fpir)
      case _ => ???
    }
  }

}

abstract class ChiselGroupElement(module: RawModule, name: String) extends ChiselElement(module, name) {

  protected val elements: Seq[Option[ChiselElement]]

  def getAnnotation(): FloorplanAnnotation = {
    if (!isCommitted) throw ChiselFloorplanException("Must commit ChiselElement before getting its annotation!")
    // The head of elements will be this module, while the remaining ones are the children
    val elementTargets = this.module.toAbsoluteTarget +: elements.map(_.get.module.toAbsoluteTarget)
    val elementTargetsMapped: Seq[Seq[InstanceTarget]] = elementTargets.map { x =>
      assert(x.isInstanceOf[InstanceTarget], "All targets must be InstanceTargets for ChiselGroupElement annotations")
      Seq(x.asInstanceOf[InstanceTarget])
    }
    FloorplanGroupAnnotation(elementTargetsMapped, fpir.asInstanceOf[Group])
  }

}

abstract class ChiselLayoutElement(module: RawModule) extends ChiselGroupElement(module, "")

final class ChiselLogicRect private[chisel] (
  module: RawModule,
  val width: Constraint[LengthUnit],
  val height: Constraint[LengthUnit],
  val area: Constraint[AreaUnit],
  val aspectRatio: Constraint[Rational],
  val hardBoundary: Boolean
) extends ChiselPrimitiveElement(module, "") {

  protected def generateElement(): Element = ConstrainedLogicRect(width, height, area, aspectRatio, hardBoundary)

}

final class ChiselPlaceholderRect private[chisel] (
  module: RawModule,
  name: String,
  val width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  val height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  val area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  val aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ChiselPrimitiveElement(module, name) {

  protected def generateElement(): Element = ConstrainedPlaceholderRect(name, width, height, area, aspectRatio)

}

final class ChiselWeightedGrid private[chisel] (
  module: RawModule,
  name: String,
  val x: Int,
  val y: Int,
  val packed: Boolean
) extends ChiselGroupElement(module, name) {

  assert(x > 0)
  assert(y > 0)

  // TODO add resizing APIs
  private var xDim = x
  private var yDim = y

  protected val elements = ArraySeq.fill[Option[ChiselElement]](xDim*yDim) { Option.empty[ChiselElement] }
  private val weights = ArraySeq.fill[Rational](xDim*yDim) { Rational(1) }

  def set(x: Int, y: Int, element: ChiselElement, weight: Rational = Rational(1)): Unit = {
    if (isCommitted) throw new ChiselFloorplanException("Cannot modify a ChiselWeightedGrid after committing")
    if (x >= xDim) throw new IndexOutOfBoundsException(s"X-value ${x} >= ${xDim} in ChiselWeightedGrid")
    if (y >= yDim) throw new IndexOutOfBoundsException(s"Y-value ${y} >= ${yDim} in ChiselWeightedGrid")
    elements(y*xDim + x) = Some(element)
    weights(y*xDim + x) = weight
  }

  def setModule(x: Int, y: Int, elementModule: RawModule, weight: Rational = Rational(1)): Unit = set(x, y, Floorplan.createRect(elementModule), weight)

  private def convertNonesToPlaceholders() = elements.transform(_.orElse(Some(Floorplan.createPlaceholderRect(module))))

  protected def generateElement(): Element = {
    convertNonesToPlaceholders()
    WeightedGrid(
      name,
      xDim,
      yDim,
      elements.map(_.get.commit()),
      weights,
      packed)
  }

}


final class ChiselConstrainedRelativePlacement private[chisel] (
  module: RawModule,
  val width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  val height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  val area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  val aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ChiselLayoutElement(module) {

  protected val elements = new ArraySeq[Option[ChiselElement]](0)
  protected val placements = new ArraySeq[RelativePlacementConstraint](0)

  def add(element: ChiselElement, constraint: RelativePlacementConstraint): Unit = {
    if (isCommitted) throw new ChiselFloorplanException("Cannot modify a ChiselConstrainedRelativePlacement after committing")
    elements :+ Some(element)
    placements :+ constraint
  }

  def addModule(elementModule: RawModule, constraint: RelativePlacementConstraint): Unit = add(Floorplan.createRect(elementModule), constraint)

  protected def generateElement(): Element = {
    ConstrainedRelativePlacement(
      elements.map(_.get.commit()),
      placements,
      width,
      height,
      area,
      aspectRatio)
  }

}