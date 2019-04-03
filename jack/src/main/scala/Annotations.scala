
package jack

import firrtl.{CircuitState, LowForm, Namespace, Transform}
import firrtl.analyses.{InstanceGraph}
import firrtl.annotations.{SingleTargetAnnotation, ModuleTarget}
import chisel3.experimental.{annotate, ChiselAnnotation, RawModule}

// TODO support InstanceTarget too
case class MeasureAreaAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  def targets = Seq(target)
  def duplicate(n: ModuleTarget) = this.copy(n)
}

object MeasureArea {

  def apply[T <: RawModule](m: T) = annotate(new ChiselAnnotation { def toFirrtl: MeasureAreaAnnotation = MeasureAreaAnnotation(m.toNamed.toTarget) })

}

abstract class AnnotatedMetricInstance(val path: Seq[String])

case class AnnotatedAreaInstance(p: Seq[String]) extends AnnotatedMetricInstance(p)

class GenerateMetricsIRPass extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val graph = new InstanceGraph(state.circuit)
    val paths = state.annotations.collect {
      case x: MeasureAreaAnnotation =>
        graph.findInstancesInHierarchy(x.target.name) map { case y => AnnotatedAreaInstance(y.map(_.name)).path }
    } reduce (_ ++ _) foreach { println(_) }

    // Return the input state (we don't change anything here)
    state
  }
}
