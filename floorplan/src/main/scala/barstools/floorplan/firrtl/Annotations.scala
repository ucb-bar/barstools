// See LICENSE for license details
package barstools.floorplan.firrtl


import barstools.floorplan.{Element, Group}
import firrtl.annotations._
import firrtl.stage.{RunFirrtlTransformAnnotation}

trait FloorplanAnnotation extends Annotation {
  val fpir: String
}

case class InstanceFloorplanAnnotation(targets: Seq[Seq[Target]], fpir: String) extends MultiTargetAnnotation with FloorplanAnnotation {
  def duplicate(t: Seq[Seq[Target]]) = this.copy(t, fpir)
}

case class MemFloorplanAnnotation(targets: Seq[Seq[Target]], fpir: String) extends MultiTargetAnnotation with FloorplanAnnotation {
  def duplicate(t: Seq[Seq[Target]]) = this.copy(t, fpir)
}

case class NoReferenceFloorplanAnnotation(target: Target, fpir: String) extends SingleTargetAnnotation[Target] with FloorplanAnnotation {
  def duplicate(t: Target) = this.copy(t, fpir)
}

object InstanceFloorplanAnnotation {
  def apply(targets: Seq[Seq[Target]], element: Element): InstanceFloorplanAnnotation = InstanceFloorplanAnnotation(targets, element.serialize)
}

object MemFloorplanAnnotation {
  def apply(targets: Seq[Seq[Target]], element: Element): MemFloorplanAnnotation = MemFloorplanAnnotation(targets, element.serialize)
}

object NoReferenceFloorplanAnnotation {
  def apply(target: Target, element: Element): NoReferenceFloorplanAnnotation = NoReferenceFloorplanAnnotation(target, element.serialize)
}

case class FloorplanIRFileAnnotation(value: String) extends NoTargetAnnotation

