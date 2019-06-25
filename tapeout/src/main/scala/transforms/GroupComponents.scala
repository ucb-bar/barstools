// See LICENSE for license details.

package barstools.tapeout.transforms

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.transforms.{GroupAnnotation, GroupComponents, DedupModules}

// This is just allowing us to order GroupComponents passes

case class GroupAnnotationStaged(stage: Int, components: Seq[ComponentName], newModule: String, newInstance: String, outputSuffix: Option[String] = None, inputSuffix: Option[String] = None) extends Annotation {
  def toGroupAnnotation = GroupAnnotation(components, newModule, newInstance, outputSuffix, inputSuffix)

  def currentModule: String = components.head.module.name

  def update(renames: RenameMap): Seq[Annotation] = {
    val newComponents = components.flatMap{c => renames.get(c).getOrElse(Seq(c))}.collect {
      case c: ComponentName => c
    }
    Seq(GroupAnnotationStaged(stage, newComponents, newModule, newInstance, outputSuffix, inputSuffix))
  }

}

class GroupAndDedupStaged extends Transform {
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = MidForm

  override def execute(state: CircuitState): CircuitState = {
    val stageOrdering = state.annotations.collect({ case a: GroupAnnotationStaged => a.stage }).distinct.sorted
    stageOrdering.foldLeft(state) { case (oldstate: CircuitState, stage: Int) =>
      // clear old annotations, then convert this stages' annotations
      val newAnnos = state.annotations.filter {
        case x: GroupAnnotation => false
        case _ => true
      } map {
        case x: GroupAnnotationStaged => if (x.stage == stage) { x.toGroupAnnotation } else { x }
        case x => x
      }
      val tmpstate = new GroupComponents().execute(oldstate.copy(annotations = newAnnos))
      val newstate = new DedupModules().execute(tmpstate)
      newstate
    }
  }
}
