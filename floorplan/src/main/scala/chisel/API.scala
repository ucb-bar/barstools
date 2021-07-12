// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule}

import firrtl.annotations.{ReferenceTarget, InstanceTarget, Annotation}

import barstools.floorplan._
import barstools.floorplan.firrtl.{FloorplanAnnotation, InstanceFloorplanAnnotation, NoReferenceFloorplanAnnotation}
import scala.collection.mutable.{ArraySeq, ArrayBuffer, HashMap, Set, HashSet}

final case class ChiselFloorplanException(message: String) extends Exception(message: String)

final class ChiselFloorplanContext private[chisel] (val root: InstanceTarget, topElement: ChiselElement) {

  private[chisel] val elementBuf = new ArrayBuffer[ChiselElement]()

  elementBuf.append(topElement)

  def elements: Seq[ChiselElement] = elementBuf.toSeq

  def createRect[T <: RawModule](module: T,
    width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
    aspectRatio: Constraint[Rational] = Unconstrained[Rational],
    hardBoundary: Boolean = true
  ) {
    val inst: InstanceTarget = module.toAbsoluteTarget.asInstanceOf[InstanceTarget]
    val name = FloorplanDatabase.getUnusedName(root, inst.instance)
    val elt = new ChiselLogicRect(root, name, inst, width, height, area, aspectRatio, hardBoundary)
    FloorplanDatabase.register(root, elt)
    elementBuf.append(elt)
  }

  def createDummy(
    name: Option[String] = None,
    width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
    aspectRatio: Constraint[Rational] = Unconstrained[Rational]
  ) {
    val nameStr = FloorplanDatabase.getUnusedName(root, name)
    val elt = new ChiselDummyRect(root, nameStr, width, height, area, aspectRatio)
    FloorplanDatabase.register(root, elt)
    elementBuf.append(elt)
  }

/*
  def grid[T <: RawModule](module: T,
    name: String,
    x: Int = 1,
    y: Int = 1,
    packed: Boolean = false
  ) = {
    val elt = new ChiselWeightedGrid(module, name, x, y, packed)
    FloorplanDatabase.register(module, elt)
    elt
  }
  */
}

object Floorplan {

  def apply[T <: RawModule](module: T,
    width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
    area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
    aspectRatio: Constraint[Rational] = Unconstrained[Rational],
    hardBoundary: Boolean = true
  ) = {
    val root: InstanceTarget = module.toAbsoluteTarget.asInstanceOf[InstanceTarget]
    val name = FloorplanDatabase.getUnusedName(root, root.ofModule)
    val elt = new ChiselLogicRect(root, name, root, width, height, area, aspectRatio, hardBoundary)
    FloorplanDatabase.register(root, elt)
    new ChiselFloorplanContext(root, elt)
  }

}

private[chisel] object FloorplanDatabase {

  private val nameMap = new HashMap[InstanceTarget, Set[String]]()
  private val elements = new HashSet[ChiselElement]()

  private def getSet(root: InstanceTarget) = nameMap.getOrElseUpdate(root, new HashSet[String])

  // TODO I'm not sure this is necessary anymore
  def register(root: InstanceTarget, element: ChiselElement): Unit = {
    val name = element.name
    val set = getSet(root)
    if (set.contains(name)) {
      throw new ChiselFloorplanException(s"Duplicate floorplan element registration ${name} for InstanceTarget "+root.asPath.toList.map(_._1.value).mkString(".")+"!")
    }
    elements.add(element)
    set.add(name)
  }

  def getUnusedName(root: InstanceTarget, suggestion: Option[String]): String = getUnusedName(root, suggestion.getOrElse("unnamed"))

  def getUnusedName(root: InstanceTarget, suggestion: String): String = {
    val set = getSet(root)
    var id = 0
    // This is slow and bad, but hopefully rare
    while (set.contains(suggestion + s"_${id}")) { id = id + 1 }
    suggestion + s"_${id}"
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

sealed abstract class ChiselElement(val root: InstanceTarget, val name: String) {
  protected def generateElement(): Element
  private[chisel] def getFloorplanAnnotations(): Seq[FloorplanAnnotation]
  def getAnnotations(): Seq[Annotation] = getFloorplanAnnotations()
}

sealed abstract class ChiselDummyElement(root: InstanceTarget, name: String) extends ChiselElement(root, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(root, generateElement()))
}

sealed abstract class ChiselPrimitiveElement(root: InstanceTarget, name: String, val instance: InstanceTarget) extends ChiselElement(root, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(InstanceFloorplanAnnotation(Seq(Seq(root), Seq(instance)), generateElement()))
}

sealed abstract class ChiselGroupElement(root: InstanceTarget, name: String) extends ChiselElement(root, name) {
  protected def elements: Seq[Option[ChiselElement]]
  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(root, generateElement()))
}

final class ChiselLogicRect private[chisel] (
  root: InstanceTarget,
  name: String,
  instance: InstanceTarget,
  val width: Constraint[LengthUnit],
  val height: Constraint[LengthUnit],
  val area: Constraint[AreaUnit],
  val aspectRatio: Constraint[Rational],
  val hardBoundary: Boolean
) extends ChiselPrimitiveElement(root, name, instance) {

  protected def generateElement(): Element = ConstrainedLogicRect(name, width, height, area, aspectRatio, hardBoundary)

}

final class ChiselDummyRect private[chisel] (
  root: InstanceTarget,
  name: String,
  val width: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  val height: Constraint[LengthUnit] = Unconstrained[LengthUnit],
  val area: Constraint[AreaUnit] = Unconstrained[AreaUnit],
  val aspectRatio: Constraint[Rational] = Unconstrained[Rational]
) extends ChiselDummyElement(root, name) {

  protected def generateElement(): Element = ConstrainedDummyRect(name, width, height, area, aspectRatio)

}

/*
final class ChiselWeightedGrid private[chisel] (
  root: InstanceTarget,
  name: String,
  val xDim: Int,
  val yDim: Int,
  val packed: Boolean
) extends ChiselGroupElement(module, name) {

  assert(xDim > 0)
  assert(yDim > 0)

  protected val elements = ArraySeq.fill[Option[ChiselElement]](xDim*yDim) { Option.empty[ChiselElement] }
  private val weights = ArraySeq.fill[Rational](xDim*yDim) { Rational(1) }

  private var _isCommitted = false

  def isCommitted = _isCommitted

  def set(x: Int, y: Int, element: ChiselElement, weight: Rational): Unit = {
    if (isCommitted) throw new ChiselFloorplanException("Cannot modify a ChiselWeightedGrid after committing")
    if (x >= xDim) throw new IndexOutOfBoundsException(s"X-value ${x} >= ${xDim} in ChiselWeightedGrid")
    if (y >= yDim) throw new IndexOutOfBoundsException(s"Y-value ${y} >= ${yDim} in ChiselWeightedGrid")
    if (elements(y*xDim + x).isDefined) throw new ChiselFloorplanException(s"Coordinates (${x},${y}) already in use")
    elements(y*xDim + x) = Some(element)
    weights(y*xDim + x) = weight
  }

  def set(x: Int, y: Int, element: ChiselElement): Unit = set(x, y, element, Rational(1))

  def set(x: Int, y: Int, eltModule: RawModule, weight: Rational): Unit = {
    // TODO what should the hardness of this boundary be?
    val element = new ChiselLogicRect(eltModule, Unconstrained[LengthUnit], Unconstrained[LengthUnit], Unconstrained[AreaUnit], Unconstrained[Rational], true)
    set(x, y, element, weight)
  }

  def set(x: Int, y: Int, root: InstanceTarget): Unit = set(x, y, module, Rational(1))

  protected def generateElement(): Element = {
    _isCommitted = true
    elements.transform(_.orElse(Some(Floorplan.dummy(module))))
    WeightedGrid(
      name,
      xDim,
      yDim,
      elements.map(_.get.name),
      weights,
      packed)
  }

}
*/

