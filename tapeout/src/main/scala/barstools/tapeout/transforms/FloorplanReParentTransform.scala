// See LICENSE for license details
package barstools.tapeout.transforms
// This needs to be in the tapeout package, not floorplan, because of the build dependencies (floorplan cannot depend on tapeout)

import barstools.floorplan.firrtl.{NoReferenceFloorplanAnnotation, InstanceFloorplanAnnotation, MemFloorplanAnnotation, FloorplanAnnotation}
import firrtl.annotations.{InstanceTarget, ReferenceTarget, ModuleTarget, Target, IsComponent}
import firrtl.{CircuitState, Transform, DependencyAPIMigration, AnnotationSeq}
import firrtl.stage.Forms
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency

class FloorplanReParentTransform extends Transform with DependencyAPIMigration {

  override def prerequisites:         Seq[TransformDependency] = Forms.HighForm
  override def optionalPrerequisites: Seq[TransformDependency] = Seq.empty
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(Dependency[ReParentCircuit])
  override def invalidates(a: Transform): Boolean = false

  def transformTarget(t: Target, top: String): Target = t match {
    case it: InstanceTarget =>
      if (it.ofModule == top) it.ofModuleTarget else it
    case ot => ot
  }

  def transformTargets(t: Seq[Seq[Target]], top: String): Seq[Seq[Target]] = {
    t.map(_.map(x => transformTarget(x, top)))
  }

  def execute(state: CircuitState): CircuitState = {
    val newTop = state.annotations.collectFirst {
      case ReParentCircuitAnnotation(x) => x.module
    }

    // Convert the annotated top to a ModuleTarget, otherwise leave them alone and let ReParentCircuit handle them
    val newAnnos = newTop.map(top => AnnotationSeq(state.annotations.toSeq.map { _ match {
      // We probably want to check the sizes of these first
      case a: NoReferenceFloorplanAnnotation =>
        a.target match {
          case t: InstanceTarget => a.copy(target = transformTarget(t, top))
          case t => a
        }
      case a: InstanceFloorplanAnnotation =>
        a.targets(0)(0) match {
          case t: InstanceTarget => a.copy(targets = transformTargets(a.targets, top))
          case t => a
        }
      case a: MemFloorplanAnnotation =>
        a.targets(0)(0) match {
          case t: InstanceTarget => a.copy(targets = transformTargets(a.targets, top))
          case t => a
        }
      case _: FloorplanAnnotation => ???
      case a => a
    }})).getOrElse(state.annotations)

    state.copy(annotations = newAnnos)
  }
}
