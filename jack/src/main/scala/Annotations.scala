
package barstools.jack

import firrtl.{CircuitState, LowForm, Namespace, Transform, AnnotationSeq}
import firrtl.analyses.{InstanceGraph}
import firrtl.annotations.{NoTargetAnnotation, SingleTargetAnnotation, Target, ModuleTarget, InstanceTarget}
import chisel3.experimental.{annotate, ChiselAnnotation, RawModule}
import scala.collection.mutable.HashMap

sealed abstract case class MetricsNamespace(val sort: Int, val name: String)
final object HeuristicNamespace extends MetricsNamespace(0, "heuristic")
final object SynthesisNamespace extends MetricsNamespace(1, "synthesis")
final object PostPlaceNamespace extends MetricsNamespace(2, "postplace")
final object PostRouteNamespace extends MetricsNamespace(3, "postroute")
final object TimerToolNamespace extends MetricsNamespace(4, "timertool")
final object PowerToolNamespace extends MetricsNamespace(5, "powertool")

trait HasMetricsMetadata {
  val namespace: String
  val key: String
}

trait HasInstanceMetricsMetadata extends HasMetricsMetadata {
  val path: Seq[String]
  def toMetricIR(indent: Int): String
}

trait MetricAnnotation[T <: Target] extends SingleTargetAnnotation[T] with HasMetricsMetadata

case class ModuleAreaAnnotation(target: ModuleTarget, key: String, namespace: String)
  extends MetricAnnotation[ModuleTarget] {
  // TODO do we need to uniquify the key here?
  def duplicate(t: ModuleTarget) = this.copy(target, key, namespace)
}

case class MetricsIRFileAnnotation(value: String) extends NoTargetAnnotation

object MetricsDB {

  val keys = new HashMap[String, Int]()

  def getNewKey(s: String): String = {
    val n = keys.getOrElse(s, 0)
    keys.update(s, n+1)
    return s"${s}_${n}"
  }

}

case class ModuleAreaMetrics(path: Seq[String], key: String, namespace: String) extends HasInstanceMetricsMetadata {

  def toMetricIR(indent: Int): String = {
    " "*indent + s"${key}:\n" +
    " "*indent + "  type: \"module area\"\n" +
    " "*indent + "  path: \"/" + path.mkString("/") + "\"\n"
  }

}

object ModuleArea {

  def apply[T <: RawModule](m: T, key: String = "", namespace: MetricsNamespace = SynthesisNamespace): Unit = {
    annotate(new ChiselAnnotation { def toFirrtl: ModuleAreaAnnotation = ModuleAreaAnnotation(m.toNamed.toTarget, MetricsDB.getNewKey(key), namespace.name) })
  }

}

class GenerateMetricsIRPass extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    state.annotations.collectFirst {
      case x: MetricsIRFileAnnotation => x.value
    } map { filename =>
      val writer = new java.io.FileWriter(filename)
      val graph = new InstanceGraph(state.circuit)
      val text = state.annotations.collect {
        case x: ModuleAreaAnnotation =>
          graph.findInstancesInHierarchy(x.target.name).zipWithIndex.map { case (path, id) => ModuleAreaMetrics(path.tail.map(_.name), s"${x.key}_${id}", x.namespace) }
      } reduce (_ ++ _) groupBy (_.namespace) foreach { case (namespace, annos) =>
        writer.write(s"${namespace}:\n" + annos.map(_.toMetricIR(2)).mkString(""))
      }
      writer.close()
    }
    // Return the input state (we don't change anything here)
    state
  }
}
