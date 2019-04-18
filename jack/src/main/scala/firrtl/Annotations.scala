// See LICENSE for license details
package barstools.jack.firrtl

import barstools.jack.HasMetricMetadata
import firrtl.{CircuitState, LowForm, Namespace, Transform, AnnotationSeq}
import firrtl.annotations.{NoTargetAnnotation, SingleTargetAnnotation, Target, ModuleTarget, ReferenceTarget}

case class MetricIRFileAnnotation(value: String) extends NoTargetAnnotation

case class MetricResultsFileAnnotation(value: String) extends NoTargetAnnotation

trait HasMetricInstance extends HasMetricMetadata {
  val instancePath: Seq[String]
  def toMetricIR(indent: Int): String
}

trait MetricAnnotation[T <: Target] extends SingleTargetAnnotation[T] with HasMetricMetadata

case class ModuleAreaAnnotation(target: ModuleTarget, key: String, namespace: String)
  extends MetricAnnotation[ModuleTarget] {
  // TODO do we need to uniquify the key here?
  def duplicate(t: ModuleTarget) = this.copy(target, key, namespace)
}

case class CriticalPathAnnotation(target: ModuleTarget, key: String, namespace: String, clock: ReferenceTarget)
  extends MetricAnnotation[ModuleTarget] {
  // TODO do we need to uniquify the key here?
  def duplicate(t: ModuleTarget) = this.copy(target, key, namespace, clock)
}

