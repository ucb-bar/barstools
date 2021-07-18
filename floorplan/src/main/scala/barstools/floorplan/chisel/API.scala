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

  def createElasticGrid(xDim: Int, yDim: Int, name: Option[String] = None): ChiselElasticGrid = {
    val nameStr = FloorplanDatabase.getUnusedName(root, name)
    val elt = new ChiselElasticGrid(root, nameStr, this, xDim, yDim)
    addElement(elt)
  }

  def createElasticArray(dim: Int, dir: Direction, name: Option[String]): ChiselElasticArray = {
    val nameStr = FloorplanDatabase.getUnusedName(root, name)
    val elt = new ChiselElasticArray(root, nameStr, this, dim, dir)
    addElement(elt)
  }

  def createElasticArray(dim: Int, name: Option[String]): ChiselElasticArray = createElasticArray(dim, Direction.Horizontal, name)
  def createElasticArray(dim: Int, dir: Direction): ChiselElasticArray = createElasticArray(dim, dir, None)
  def createElasticArray(dim: Int): ChiselElasticArray = createElasticArray(dim, Direction.Horizontal, None)
  def createElasticArray(elts: Seq[ChiselElement], dir: Direction = Direction.Horizontal, name: Option[String] = None): ChiselElasticArray = {
    val ary = createElasticArray(elts.length, dir, name)
    elts.zipWithIndex.foreach { case (e, i) => ary.placeElementAt(e, i) }
    ary
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

trait Direction {
  def ifH[T](h: T, v: T): T = this match {
    case Direction.Horizontal => h
    case Direction.Vertical => v
  }
  def ifV[T](h: T, v: T): T = ifH(v, h)
}

object Direction {
  case object Vertical extends Direction
  case object Horizontal extends Direction
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
  protected val elements = Seq.fill(initialSize)(Option.empty[ChiselElement]).toBuffer
  protected var isCommitted = false

  protected def generateGroupElement(names: Seq[Option[String]]): Group

  protected def generateElement(): Group = {
    isCommitted = true
    generateGroupElement(elements.map(_.map(_.name)))
  }

  private[chisel] def _placeElementAt(e: ChiselElement, idx: Int) {
    assert(!isCommitted, "Cannot add elements after committing")
    // This is only supported in scala 2.13
    //elements.padToInPlace(idx+1, None)
    for (i <- elements.length until (idx+1)) elements += None
    assert(!elements(idx).isDefined, "Already added at this location")
    elements(idx) = Some(e)
  }

  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(root, generateElement()))
}

abstract class ChiselArrayElement(root: Target, name: String, context: ChiselFloorplanContext) extends ChiselGroupElement(root, name, context) {

  private[chisel] def addElement(e: ChiselElement) {
    assert(!isCommitted, "Cannot add elements after committing")
    elements += Some(e)
  }

}

abstract class ChiselGridElement(root: Target, name: String, context: ChiselFloorplanContext, val xDim: Int, val yDim: Int) extends ChiselGroupElement(root, name, context) {

  protected def initialSize = xDim * yDim
  protected def toIdx(x: Int, y: Int): Int = xDim*y + x

  def placeElementAt(e: ChiselElement, x: Int, y: Int) { _placeElementAt(e, toIdx(x, y)) }

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
) extends ChiselArrayElement(root, name, context) {
  protected def initialSize = 0
  protected def generateGroupElement(names: Seq[Option[String]]): Group = MemElementArray(name, names)

  def addMem[T <: Data](mem: MemBase[T]) = this.addElement(this.context.addMem(mem))
}

class ChiselElasticGrid private[chisel] (
  root: Target,
  name: String,
  context: ChiselFloorplanContext,
  xDim: Int,
  yDim: Int
) extends ChiselGridElement(root, name, context, xDim, yDim) {
  final protected def generateGroupElement(names: Seq[Option[String]]): Group = ElasticGrid(name, xDim, yDim, names)
}

class ChiselElasticArray private[chisel] (
  root: Target,
  name: String,
  context: ChiselFloorplanContext,
  dim: Int,
  val dir: Direction
) extends ChiselElasticGrid(root, name, context, dir.ifH(dim,1), dir.ifV(dim,1)) {
  def placeElementAt(e: ChiselElement, i: Int) { placeElementAt(e, dir.ifH(i,0), dir.ifV(0,i)) }
}
