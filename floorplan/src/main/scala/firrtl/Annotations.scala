// See LICENSE for license details
package barstools.floorplan.firrtl


import barstools.floorplan.{Element, Group}
import firrtl.annotations._
import firrtl.stage.{RunFirrtlTransformAnnotation}

// John '21: We're going to get rid of ModuleTarget support for now. InstanceTargets may break dedup, but they don't support heterogeneous configs and are
// kind of redundant with InstanceTargets. InstanceTarget behavior is a little more intuitive for writing the Aspects.

// John '20: To make this a bit easier, I'm going to make floorplan IR embedded in this annotation rather than relying on
// the annotation to serialize the case class correctly (it doesn't currently serialize type parameters, which makes this a bit painful)
// We'll probably want to change this later
trait FloorplanAnnotation extends Annotation {
  val fpir: String
}

case class InstanceFloorplanAnnotation(targets: Seq[Seq[Target]], fpir: String) extends MultiTargetAnnotation with FloorplanAnnotation {

/*
  assert(targets.length == 2, "InstanceFloorplanAnnotation requires 2 targets")
  assert(targets(0)(0).isInstanceOf[InstanceTarget], "Root must be an InstanceTarget")
  assert(targets(1)(0).isInstanceOf[InstanceTarget], "Instance must be an InstanceTarget")
*/

  def duplicate(t: Seq[Seq[Target]]) = {
    this.copy(t, fpir)
  }
}

case class ReferenceFloorplanAnnotation(targets: Seq[Seq[Target]], fpir: String) extends MultiTargetAnnotation with FloorplanAnnotation {

/*
  assert(targets.length == 2, "InstanceFloorplanAnnotation requires 2 targets")
  assert(targets(0)(0).isInstanceOf[InstanceTarget], "Root must be an InstanceTarget")
  assert(targets(1)(0).isInstanceOf[ReferenceTarget], "Ref must be an ReferenceTarget")
*/

  def duplicate(t: Seq[Seq[Target]]) = {
    this.copy(t, fpir)
  }
}

case class NoReferenceFloorplanAnnotation(target: InstanceTarget, fpir: String) extends SingleTargetAnnotation[InstanceTarget] with FloorplanAnnotation {
  def duplicate(t: InstanceTarget) = this.copy(t, fpir)
}

object InstanceFloorplanAnnotation {
  def apply(targets: Seq[Seq[Target]], element: Element): InstanceFloorplanAnnotation = InstanceFloorplanAnnotation(targets, element.serialize)
}

object ReferenceFloorplanAnnotation {
  def apply(targets: Seq[Seq[Target]], element: Element): ReferenceFloorplanAnnotation = ReferenceFloorplanAnnotation(targets, element.serialize)
}

object NoReferenceFloorplanAnnotation {
  def apply(target: InstanceTarget, element: Element): NoReferenceFloorplanAnnotation = NoReferenceFloorplanAnnotation(target, element.serialize)
}

case class FloorplanIRFileAnnotation(value: String) extends NoTargetAnnotation

