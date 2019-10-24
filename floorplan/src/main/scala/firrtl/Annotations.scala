// See LICENSE for license details
package barstools.floorplan.firrtl


import barstools.floorplan.{Element}
import firrtl.annotations.{NoTargetAnnotation, SingleTargetAnnotation, Target, ModuleTarget, InstanceTarget, ReferenceTarget}

// John: Right now we are using ModuleTarget, which will mean that all instances of the same master
// will have the same floorplan. This is probably OK for now, but eventually we will want to support
// instance targets as well, which have some deduping issues.

// John: Another note. To make this a bit easier, I'm going to make floorplan IR embedded in this annotation rather than relying on
// the annotation to serialize the case class correctly (it doesn't currently serialize type parameters, which makes this a bit painful)
// We'll probably want to change this later
case class FloorplanModuleAnnotation(target: ModuleTarget, fpir: String) extends SingleTargetAnnotation[ModuleTarget] {
  def duplicate(t: ModuleTarget) = this.copy(target, fpir)
}

case class FloorplanIRFileAnnotation(value: String) extends NoTargetAnnotation
