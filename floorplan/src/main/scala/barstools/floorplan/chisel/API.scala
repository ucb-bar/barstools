// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule, MemBase, Data}

import firrtl.annotations.{ReferenceTarget, InstanceTarget, ModuleTarget, Target, Annotation}

import barstools.floorplan._
import barstools.floorplan.firrtl.{FloorplanAnnotation, MemFloorplanAnnotation, InstanceFloorplanAnnotation, NoReferenceFloorplanAnnotation}
import scala.collection.mutable.{ArraySeq, ArrayBuffer, HashMap, Set, HashSet}
import scala.math.{BigInt, BigDecimal}

final case class ChiselFloorplanException(message: String) extends Exception(message: String)

final class ChiselFloorplanContext private[chisel] (val root: Target, topElement: ChiselElement) {

  private[chisel] val elementBuf = new ArrayBuffer[ChiselElement]()

  elementBuf.append(topElement)

  private def addElement[T <: ChiselElement](e: T): T = {
    FloorplanDatabase.register(root, e)
    elementBuf.append(e)
    e
  }

  def elements: Seq[ChiselElement] = elementBuf.toSeq

  def createRect[T <: RawModule](module: T,
    width: Constraint = Unconstrained(),
    height: Constraint = Unconstrained(),
    area: Constraint = Unconstrained(),
    aspectRatio: Constraint = Unconstrained(),
    hardBoundary: Boolean = true
  ): ChiselLogicRect = {
    val inst: Target = module.toAbsoluteTarget
    val name = FloorplanDatabase.getUnusedName(root, inst)
    val elt = new ChiselLogicRect(root, name, inst, width, height, area, aspectRatio, hardBoundary)
    addElement(elt)
  }

  def createSpacer(
    name: Option[String] = None,
    width: Constraint = Unconstrained(),
    height: Constraint = Unconstrained(),
    area: Constraint = Unconstrained(),
    aspectRatio: Constraint = Unconstrained()
  ): ChiselSpacerRect = {
    val nameStr = FloorplanDatabase.getUnusedName(root, name)
    val elt = new ChiselSpacerRect(root, nameStr, width, height, area, aspectRatio)
    addElement(elt)
  }

  def createMemArray(name: Option[String] = None): ChiselMemArray = {
    val nameStr = FloorplanDatabase.getUnusedName(root, name)
    val elt = new ChiselMemArray(root, nameStr, this)
    addElement(elt)
  }

  def addHier[T <: RawModule](module: T): ChiselHierarchicalBarrier = {
    val inst = module.toAbsoluteTarget.asInstanceOf[InstanceTarget]
    val name = FloorplanDatabase.getUnusedName(root, inst)
    val elt = new ChiselHierarchicalBarrier(root, name, inst)
    addElement(elt)
  }

  private[chisel] def addMem[T <: Data](mem: MemBase[T]): ChiselMem = {
    val ref = mem.toAbsoluteTarget
    val name = FloorplanDatabase.getUnusedName(root, ref)
    val elt = new ChiselMem(root, name, ref)
    addElement(elt)
  }

}

object Floorplan {

  def apply[T <: RawModule](module: T,
    width: Constraint = Unconstrained(),
    height: Constraint = Unconstrained(),
    area: Constraint = Unconstrained(),
    aspectRatio: Constraint = Unconstrained(),
    hardBoundary: Boolean = true,
    margins: Margins = Margins.empty
  ) = {
    val root: Target = module.toAbsoluteTarget
    val modName = root match {
      case r: InstanceTarget => r.ofModule
      case r: ModuleTarget => r.module
      case _ => ???
    }
    val name = FloorplanDatabase.getUnusedName(root, modName)
    val elt = new ChiselHierarchicalTop(root, name, width, height, area, aspectRatio, margins, hardBoundary)
    FloorplanDatabase.register(root, elt)
    new ChiselFloorplanContext(root, elt)
  }

}

private[chisel] object FloorplanDatabase {

  private val nameMap = new HashMap[Target, Set[String]]()
  private val elements = new HashSet[ChiselElement]()

  private def getSet(root: Target) = nameMap.getOrElseUpdate(root, new HashSet[String])

  // TODO I'm not sure this is necessary anymore
  def register(root: Target, element: ChiselElement): Unit = {
    val name = element.name
    val set = getSet(root)
    if (set.contains(name)) {
      throw new ChiselFloorplanException(s"Duplicate floorplan element registration ${name} for Target "+root.toString+"!")
    }
    elements.add(element)
    set.add(name)
  }

  def getUnusedName(root: Target): String = getUnusedName(root, None)

  def getUnusedName(root: Target, suggestion: Option[String]): String = getUnusedName(root, suggestion.getOrElse("unnamed"))

  def getUnusedName(root: Target, suggestion: String): String = {
    val set = getSet(root)
    var id = 0
    // This is slow and bad, but hopefully rare
    while (set.contains(suggestion + s"_${id}")) { id = id + 1 }
    suggestion + s"_${id}"
  }

  def getUnusedName(root: Target, inst: Target): String = {
    val instName = inst match {
      case t: InstanceTarget => t.instance
      case t: ModuleTarget => t.module
      case t: ReferenceTarget => t.ref
      case _ => ???
    }
    getUnusedName(root, instName)
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

  def area(x: Double): BigDecimal = {
    unit.map { u =>
      BigDecimal(scala.math.round(u*x*x))
    }.getOrElse {
      throw new FloorplanUnitsException("Cannot create floorplan with concrete units- units have not been set! Call FloorplanUnits.set first.")
    }
  }

  def length(x: Double): BigDecimal = {
    unit.map { u =>
      BigDecimal(scala.math.round(u*x))
    }.getOrElse {
      throw new FloorplanUnitsException("Cannot create floorplan with concrete units- units have not been set! Call FloorplanUnits.set first.")
    }
  }

}

sealed abstract class ChiselElement(val root: Target, val name: String) {
  protected def generateElement(): Element
  private[chisel] def getFloorplanAnnotations(): Seq[FloorplanAnnotation]
  def getAnnotations(): Seq[Annotation] = getFloorplanAnnotations()
}

sealed abstract class ChiselNoReferenceElement(root: Target, name: String) extends ChiselElement(root, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(root, generateElement()))
}

sealed abstract class ChiselInstanceElement(root: Target, name: String, val instance: Target) extends ChiselElement(root, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(InstanceFloorplanAnnotation(Seq(Seq(root), Seq(instance)), generateElement()))
}

sealed abstract class ChiselMemElement(root: Target, name: String, val reference: ReferenceTarget) extends ChiselElement(root, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(MemFloorplanAnnotation(Seq(Seq(root), Seq(reference)), generateElement()))
}

sealed abstract class ChiselGroupElement(root: Target, name: String, val context: ChiselFloorplanContext) extends ChiselElement(root, name) {
  protected def initialSize: Int
  private val elements = Seq.fill(initialSize)(Option.empty[ChiselElement]).toBuffer
  private var isCommitted = false

  protected def generateGroupElement(names: Seq[Option[String]]): Group

  protected def generateElement(): Group = {
    isCommitted = true
    generateGroupElement(elements.map(_.map(_.name)))
  }

  private[chisel] def placeElementAt(e: ChiselElement, idx: Int) {
    assert(!isCommitted, "Cannot add elements after committing")
    // This is only supported in scala 2.13
    //elements.padToInPlace(idx+1, None)
    for (i <- elements.length until (idx+1)) elements += None
    elements(idx) = Some(e)
  }

  private[chisel] def addElement(e: ChiselElement) {
    assert(!isCommitted, "Cannot add elements after committing")
    elements += Some(e)
  }

  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(root, generateElement()))
}

final class ChiselHierarchicalTop private[chisel] (
  root: Target,
  name: String,
  val width: Constraint,
  val height: Constraint,
  val area: Constraint,
  val aspectRatio: Constraint,
  val margins: Margins,
  val hardBoundary: Boolean
) extends ChiselNoReferenceElement(root, name) {
  val ofModule = root match {
    case t: InstanceTarget => t.ofModule
    case t: ModuleTarget => t.module
    case _ => ???
  }
  protected def generateElement(): Element = ConstrainedHierarchicalTop(name, ofModule, width, height, area, aspectRatio, margins, hardBoundary)
}

final class ChiselHierarchicalBarrier private[chisel] (
  root: Target,
  name: String,
  instance: InstanceTarget
) extends ChiselInstanceElement(root, name, instance) {
  protected def generateElement(): Element = HierarchicalBarrier(name, instance.ofModule)
}

final class ChiselLogicRect private[chisel] (
  root: Target,
  name: String,
  instance: Target,
  val width: Constraint,
  val height: Constraint,
  val area: Constraint,
  val aspectRatio: Constraint,
  val hardBoundary: Boolean
) extends ChiselInstanceElement(root, name, instance) {
  protected def generateElement(): Element = ConstrainedLogicRect(name, width, height, area, aspectRatio, hardBoundary)
}

final class ChiselSpacerRect private[chisel] (
  root: Target,
  name: String,
  val width: Constraint = Unconstrained(),
  val height: Constraint = Unconstrained(),
  val area: Constraint = Unconstrained(),
  val aspectRatio: Constraint = Unconstrained()
) extends ChiselNoReferenceElement(root, name) {
  protected def generateElement(): Element = ConstrainedSpacerRect(name, width, height, area, aspectRatio)
}

final class ChiselMem private[chisel] (
  root: Target,
  name: String,
  reference: ReferenceTarget
) extends ChiselMemElement(root, name, reference) {
  protected def generateElement(): Element = MemElement(name)
}

final class ChiselMemArray private[chisel] (
  root: Target,
  name: String,
  context: ChiselFloorplanContext
) extends ChiselGroupElement(root, name, context) {
  protected def initialSize = 0
  protected def generateGroupElement(names: Seq[Option[String]]): Group = MemElementArray(name, names)

  def addMem[T <: Data](mem: MemBase[T]) = this.addElement(this.context.addMem(mem))
}


/*
final class ChiselWeightedGrid private[chisel] (
  root: Target,
  name: String,
  val xDim: Int,
  val yDim: Int,
  val packed: Boolean
) extends ChiselGroupElement(module, name) {

  assert(xDim > 0)
  assert(yDim > 0)

  protected val elements = ArraySeq.fill[Option[ChiselElement]](xDim*yDim) { Option.empty[ChiselElement] }
  private val weights = ArraySeq.fill[BigDecimal](xDim*yDim) { BigDecimal(1) }

  private var _isCommitted = false

  def isCommitted = _isCommitted

  def set(x: Int, y: Int, element: ChiselElement, weight: BigDecimal): Unit = {
    if (isCommitted) throw new ChiselFloorplanException("Cannot modify a ChiselWeightedGrid after committing")
    if (x >= xDim) throw new IndexOutOfBoundsException(s"X-value ${x} >= ${xDim} in ChiselWeightedGrid")
    if (y >= yDim) throw new IndexOutOfBoundsException(s"Y-value ${y} >= ${yDim} in ChiselWeightedGrid")
    if (elements(y*xDim + x).isDefined) throw new ChiselFloorplanException(s"Coordinates (${x},${y}) already in use")
    elements(y*xDim + x) = Some(element)
    weights(y*xDim + x) = weight
  }

  def set(x: Int, y: Int, element: ChiselElement): Unit = set(x, y, element, BigDecimal(1))

  def set(x: Int, y: Int, eltModule: RawModule, weight: BigDecimal): Unit = {
    // TODO what should the hardness of this boundary be?
    val element = new ChiselLogicRect(eltModule, Unconstrained(), Unconstrained(), Unconstrained(), Unconstrained(), true)
    set(x, y, element, weight)
  }

  def set(x: Int, y: Int, root: Target): Unit = set(x, y, module, BigDecimal(1))

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

