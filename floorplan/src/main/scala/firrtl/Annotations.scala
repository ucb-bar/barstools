// See LICENSE for license details
package barstools.floorplan.firrtl


import barstools.floorplan.{Element, Group}
import firrtl.annotations._
import firrtl.stage.{RunFirrtlTransformAnnotation}

// John: Right now we are using ModuleTarget, which will mean that all instances of the same master
// will have the same floorplan. This is probably OK for now, but eventually we will want to support
// instance targets as well, which have some deduping issues.

// John: Another note. To make this a bit easier, I'm going to make floorplan IR embedded in this annotation rather than relying on
// the annotation to serialize the case class correctly (it doesn't currently serialize type parameters, which makes this a bit painful)
// We'll probably want to change this later
trait FloorplanAnnotation extends Annotation {
  val fpir: String
}

case class FloorplanModuleAnnotation(target: ModuleTarget, fpir: String) extends SingleTargetAnnotation[ModuleTarget] with FloorplanAnnotation {
  def duplicate(t: ModuleTarget) = this.copy(t, fpir)
}

case class FloorplanInstanceAnnotation(target: InstanceTarget, fpir: String) extends SingleTargetAnnotation[InstanceTarget] with FloorplanAnnotation {
  def duplicate(t: InstanceTarget) = this.copy(t, fpir)
}

case class FloorplanGroupAnnotation(targets: Seq[Seq[Target]], fpir: String) extends MultiTargetAnnotation with FloorplanAnnotation {
  def duplicate(t: Seq[Seq[Target]]) = this.copy(t, fpir)
}

object FloorplanModuleAnnotation {
  def apply(target: ModuleTarget, element: Element): FloorplanModuleAnnotation = FloorplanModuleAnnotation(target, element.serialize)
}

object FloorplanInstanceAnnotation {
  def apply(target: InstanceTarget, element: Element): FloorplanInstanceAnnotation = FloorplanInstanceAnnotation(target, element.serialize)
}

object FloorplanGroupAnnotation {
  //def apply(targets: Seq[Target], fpir: String): FloorplanGroupAnnotation = FloorplanGroupAnnotation(targets.map(Seq(_)), fpir)
  //def apply(targets: Seq[Target], element: Element): FloorplanGroupAnnotation = FloorplanGroupAnnotation(targets, element.serialize)
  def apply(targets: Seq[Seq[Target]], element: Group): FloorplanGroupAnnotation = FloorplanGroupAnnotation(targets, element.serialize)
}

case class FloorplanIRFileAnnotation(value: String) extends NoTargetAnnotation

object GenerateFloorplanIR {
  private var emitted = false

  def emit(): Seq[RunFirrtlTransformAnnotation] = {
    if (emitted) Nil else {
      emitted = true
      Seq(RunFirrtlTransformAnnotation(new GenerateFloorplanIRPass))
    }
  }
}
