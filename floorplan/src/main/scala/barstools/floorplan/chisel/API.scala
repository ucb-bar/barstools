// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule, MemBase, Data}

import firrtl.annotations.{ReferenceTarget, InstanceTarget, ModuleTarget, Target, Annotation}

import barstools.floorplan._
import barstools.floorplan.firrtl.{FloorplanAnnotation, MemFloorplanAnnotation, InstanceFloorplanAnnotation, NoReferenceFloorplanAnnotation}
import scala.collection.mutable.{ArraySeq, ArrayBuffer, HashMap, Set, HashSet}
import scala.math.{BigInt, BigDecimal}

final case class ChiselFloorplanException(message: String) extends Exception(message: String)

final class ChiselFloorplanContext private[chisel] (val scope: Target, val topElement: ChiselHierarchicalTop) {

  private[chisel] val elementBuf = new ArrayBuffer[ChiselElement]()

  elementBuf.append(topElement)

  private def addElement[T <: ChiselElement](e: T): T = {
    FloorplanDatabase.register(scope, e)
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
    val inst = module.toAbsoluteTarget.asInstanceOf[InstanceTarget]
    val name = FloorplanDatabase.getUnusedName(scope, inst)
    val elt = new ChiselLogicRect(scope, name, inst, width, height, area, aspectRatio, hardBoundary)
    addElement(elt)
  }

  def createSpacer(
    name: Option[String] = None,
    width: Constraint = Unconstrained(),
    height: Constraint = Unconstrained(),
    area: Constraint = Unconstrained(),
    aspectRatio: Constraint = Unconstrained()
  ): ChiselSpacerRect = {
    val nameStr = FloorplanDatabase.getUnusedName(scope, name)
    val elt = new ChiselSpacerRect(scope, nameStr, width, height, area, aspectRatio)
    addElement(elt)
  }

  def createElasticGrid(xDim: Int, yDim: Int, name: Option[String] = None): ChiselElasticGrid = {
    val nameStr = FloorplanDatabase.getUnusedName(scope, name)
    val elt = new ChiselElasticGrid(scope, nameStr, this, xDim, yDim)
    addElement(elt)
  }

  def createElasticArray(dim: Int, dir: Direction, name: Option[String]): ChiselElasticArray = {
    val nameStr = FloorplanDatabase.getUnusedName(scope, name)
    val elt = new ChiselElasticArray(scope, nameStr, this, dim, dir)
    addElement(elt)
  }

  def createElasticArray(dim: Int, name: Option[String]): ChiselElasticArray = createElasticArray(dim, Direction.Horizontal, name)
  def createElasticArray(dim: Int, dir: Direction): ChiselElasticArray = createElasticArray(dim, dir, None)
  def createElasticArray(dim: Int): ChiselElasticArray = createElasticArray(dim, Direction.Horizontal, None)
  def createElasticArray(elts: Seq[ChiselElement], dir: Direction = Direction.Horizontal, name: Option[String] = None): ChiselElasticArray = {
    val ary = createElasticArray(elts.length, dir, name)
    elts.zipWithIndex.foreach { case (e, i) => ary.placeAt(i, e) }
    ary
  }

  def createMemArray(name: Option[String] = None): ChiselMemArray = {
    val nameStr = FloorplanDatabase.getUnusedName(scope, name)
    val elt = new ChiselMemArray(scope, nameStr, this)
    addElement(elt)
  }

  def addHier[T <: RawModule](module: T): ChiselHierarchicalBarrier = {
    val inst = module.toAbsoluteTarget.asInstanceOf[InstanceTarget]
    val name = FloorplanDatabase.getUnusedName(scope, inst)
    val elt = new ChiselHierarchicalBarrier(scope, name, inst)
    addElement(elt)
  }

  private[chisel] def addMem[T <: Data](mem: MemBase[T]): ChiselMem = {
    val ref = mem.toAbsoluteTarget
    val name = FloorplanDatabase.getUnusedName(scope, ref)
    val elt = new ChiselMem(scope, name, ref)
    addElement(elt)
  }

  def setTopGroup[T <: ChiselGroupElement](g: T): T = {
    topElement.setTopGroup(g)
    g
  }

}

object Floorplan {


  def apply[T <: RawModule](module: T): ChiselFloorplanContext = apply(
    module = module,
    width = Unconstrained(),
    height = Unconstrained(),
    area = Unconstrained(),
    aspectRatio = Unconstrained(),
    hardBoundary = true,
    margins = Margins.empty
  )

  def apply[T <: RawModule](module: T,
    width: Constraint,
    height: Constraint,
    area: Constraint,
    aspectRatio: Constraint,
    hardBoundary: Boolean,
    margins: Margins
  ): ChiselFloorplanContext = {
    val scope: Target = module.toAbsoluteTarget
    val modName = scope match {
      case r: InstanceTarget => r.ofModule
      case r: ModuleTarget => r.module
      case _ => ???
    }
    val name = FloorplanDatabase.getUnusedName(scope, modName)
    val elt = new ChiselHierarchicalTop(scope, name, width, height, area, aspectRatio, margins, hardBoundary)
    FloorplanDatabase.register(scope, elt)
    new ChiselFloorplanContext(scope, elt)
  }

  def apply[T <: RawModule](module: T,
    width: Double,
    height: Double
  ): ChiselFloorplanContext = apply(
    module = module,
    width = EqualTo(BigDecimal(width)),
    height = EqualTo(BigDecimal(height)),
    area = Unconstrained(),
    aspectRatio = Unconstrained(),
    hardBoundary = true,
    margins = Margins.empty
  )

  def apply[T <: RawModule](module: T,
    width: String,
    height: String
  ): ChiselFloorplanContext = apply(
    module = module,
    width = EqualTo(BigDecimal(width)),
    height = EqualTo(BigDecimal(height)),
    area = Unconstrained(),
    aspectRatio = Unconstrained(),
    hardBoundary = true,
    margins = Margins.empty
  )

}

private[chisel] object FloorplanDatabase {

  private val nameMap = new HashMap[Target, Set[String]]()
  private val elements = new HashSet[ChiselElement]()

  private def getSet(scope: Target) = nameMap.getOrElseUpdate(scope, new HashSet[String])

  // TODO I'm not sure this is necessary anymore
  def register(scope: Target, element: ChiselElement): Unit = {
    val name = element.name
    val set = getSet(scope)
    if (set.contains(name)) {
      throw new ChiselFloorplanException(s"Duplicate floorplan element registration ${name} for Target "+scope.toString+"!")
    }
    elements.add(element)
    set.add(name)
  }

  def getUnusedName(scope: Target): String = getUnusedName(scope, None)

  def getUnusedName(scope: Target, suggestion: Option[String]): String = getUnusedName(scope, suggestion.getOrElse("unnamed"))

  def getUnusedName(scope: Target, suggestion: String): String = {
    val set = getSet(scope)
    var id = 0
    // This is slow and bad, but hopefully rare
    while (set.contains(suggestion + s"_${id}")) { id = id + 1 }
    suggestion + s"_${id}"
  }

  def getUnusedName(scope: Target, inst: Target): String = {
    val instName = inst match {
      case t: InstanceTarget => t.instance
      case t: ModuleTarget => t.module
      case t: ReferenceTarget => t.ref
      case _ => ???
    }
    getUnusedName(scope, instName)
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

sealed abstract class ChiselElement(val scope: Target, val name: String) {
  protected def generateElement(): Element
  private[chisel] def getFloorplanAnnotations(): Seq[FloorplanAnnotation]
  def getAnnotations(): Seq[Annotation] = getFloorplanAnnotations()
  private var parent = Option.empty[CanBeParent]
  private[chisel] def setParent(p: CanBeParent) {
    assert(!this.parent.isDefined, "Element cannot have multiple parents")
    this.parent = Some(p)
  }
  protected def parentName: String = {
    assert(this.parent.isDefined, "Parent is not defined for this element")
    this.parent.get.name
  }
}

sealed trait CanBeParent {
  this: ChiselElement =>
  def name: String
}

sealed abstract class ChiselNoReferenceElement(scope: Target, name: String) extends ChiselElement(scope, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(scope, generateElement()))
}

sealed abstract class ChiselInstanceElement(scope: Target, name: String, val instance: Target) extends ChiselElement(scope, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(InstanceFloorplanAnnotation(Seq(Seq(scope), Seq(instance)), generateElement()))
}

sealed abstract class ChiselMemElement(scope: Target, name: String, val reference: ReferenceTarget) extends ChiselElement(scope, name) {
  private[chisel] def getFloorplanAnnotations() = Seq(MemFloorplanAnnotation(Seq(Seq(scope), Seq(reference)), generateElement()))
}

sealed abstract class ChiselGroupElement(scope: Target, name: String, val context: ChiselFloorplanContext)
  extends ChiselElement(scope, name) with CanBeParent {

  protected def initialSize: Int
  protected val elements = Seq.fill(initialSize)(Option.empty[ChiselElement]).toBuffer
  protected var isCommitted = false

  protected def generateGroupElement(names: Seq[Option[String]]): Group

  protected def generateElement(): Group = {
    isCommitted = true
    generateGroupElement(elements.map(_.map(_.name)))
  }

  private[chisel] def _placeAt[T <: ChiselElement](idx: Int, e: T): T = {
    assert(!isCommitted, "Cannot add elements after committing")
    // This is only supported in scala 2.13
    //elements.padToInPlace(idx+1, None)
    for (i <- elements.length until (idx+1)) elements += None
    assert(!elements(idx).isDefined, "Already added at this location")
    elements(idx) = Some(e)
    e.setParent(this)
    e
  }

  private[chisel] def getFloorplanAnnotations() = Seq(NoReferenceFloorplanAnnotation(scope, generateElement()))
}

abstract class ChiselArrayElement(scope: Target, name: String, context: ChiselFloorplanContext) extends ChiselGroupElement(scope, name, context) {

  private[chisel] def addElement(e: ChiselElement) {
    assert(!isCommitted, "Cannot add elements after committing")
    elements += Some(e)
    e.setParent(this)
  }

}

abstract class ChiselGridElement(scope: Target, name: String, context: ChiselFloorplanContext, val xDim: Int, val yDim: Int) extends ChiselGroupElement(scope, name, context) {

  protected def initialSize = xDim * yDim
  protected def toIdx(x: Int, y: Int): Int = xDim*y + x

  def placeAt[T <: ChiselElement](x: Int, y: Int, e: T): T = _placeAt(toIdx(x, y), e)

}

final class ChiselHierarchicalTop private[chisel] (
  scope: Target,
  name: String,
  val width: Constraint,
  val height: Constraint,
  val area: Constraint,
  val aspectRatio: Constraint,
  val margins: Margins,
  val hardBoundary: Boolean
) extends ChiselNoReferenceElement(scope, name) with CanBeParent {
  private var topGroup = Option.empty[ChiselGroupElement]

  private def topGroupName: String = {
    assert(this.topGroup.isDefined, "HierarchicalTop needs a topGroup")
    this.topGroup.get.name
  }

  protected def generateElement(): Element = ConstrainedHierarchicalTop(name, topGroupName, width, height, area, aspectRatio, margins, hardBoundary)

  private[chisel] def setTopGroup(t: ChiselGroupElement) {
    assert(!this.topGroup.isDefined, "Cannot set topGroup twice")
    t.setParent(this)
    this.topGroup = Some(t)
  }
}

final class ChiselHierarchicalBarrier private[chisel] (
  scope: Target,
  name: String,
  instance: InstanceTarget
) extends ChiselInstanceElement(scope, name, instance) {
  protected def generateElement(): Element = HierarchicalBarrier(name, parentName)
}

final class ChiselLogicRect private[chisel] (
  scope: Target,
  name: String,
  instance: InstanceTarget,
  val width: Constraint,
  val height: Constraint,
  val area: Constraint,
  val aspectRatio: Constraint,
  val hardBoundary: Boolean
) extends ChiselInstanceElement(scope, name, instance) {
  protected def generateElement(): Element = ConstrainedLogicRect(name, parentName, width, height, area, aspectRatio, hardBoundary)
}

final class ChiselSpacerRect private[chisel] (
  scope: Target,
  name: String,
  val width: Constraint = Unconstrained(),
  val height: Constraint = Unconstrained(),
  val area: Constraint = Unconstrained(),
  val aspectRatio: Constraint = Unconstrained()
) extends ChiselNoReferenceElement(scope, name) {
  protected def generateElement(): Element = ConstrainedSpacerRect(name, parentName, width, height, area, aspectRatio)
}

final class ChiselMem private[chisel] (
  scope: Target,
  name: String,
  reference: ReferenceTarget
) extends ChiselMemElement(scope, name, reference) {
  protected def generateElement(): Element = MemElement(name, parentName)
}

final class ChiselMemArray private[chisel] (
  scope: Target,
  name: String,
  context: ChiselFloorplanContext
) extends ChiselArrayElement(scope, name, context) {
  protected def initialSize = 0
  protected def generateGroupElement(names: Seq[Option[String]]): Group = MemElementArray(name, parentName, names)

  def addMem[T <: Data](mem: MemBase[T]) = this.addElement(this.context.addMem(mem))
}

class ChiselElasticGrid private[chisel] (
  scope: Target,
  name: String,
  context: ChiselFloorplanContext,
  xDim: Int,
  yDim: Int
) extends ChiselGridElement(scope, name, context, xDim, yDim) {
  final protected def generateGroupElement(names: Seq[Option[String]]): Group = ConstrainedElasticGrid(
    name,
    parentName,
    xDim,
    yDim,
    names,
    UnconstrainedSeq(xDim*yDim),
    UnconstrainedSeq(xDim*yDim),
    UnconstrainedSeq(xDim*yDim),
    UnconstrainedSeq(xDim*yDim))
}

class ChiselElasticArray private[chisel] (
  scope: Target,
  name: String,
  context: ChiselFloorplanContext,
  dim: Int,
  val dir: Direction
) extends ChiselElasticGrid(scope, name, context, dir.ifH(dim,1), dir.ifV(dim,1)) {
  def placeAt[T <: ChiselElement](i: Int, e: T): T = placeAt(dir.ifH(i,0), dir.ifV(0,i), e)
}
