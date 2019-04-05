// See LICENSE for license details
package barstools.jack

import firrtl.{CircuitState, LowForm, Namespace, Transform, AnnotationSeq}
import firrtl.analyses.{InstanceGraph}

case class ModuleAreaMetric(instancePath: Seq[String], key: String, namespace: String) extends HasMetricInstance {
  def toMetricIR(indent: Int): String = {
    " "*indent + s"${key}:\n" +
    " "*indent + "  type: \"area\"\n" +
    " "*indent + "  module: \"/" + instancePath.mkString("/") + "\"\n"
  }
}

case class CriticalPathMetric(instancePath: Seq[String], key: String, namespace: String) extends HasMetricInstance {
  def toMetricIR(indent: Int): String = {
    " "*indent + s"${key}:\n" +
    " "*indent + "  type: \"critical path\"\n" +
    " "*indent + "  module: \"/" + instancePath.mkString("/") + "\"\n" +
    " "*indent + "  clock: \":clock\""
    //" "*indent + "  clock: \"" + clockSpec.toString + "\"\n"
  }
}

class GenerateMetricIRPass extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    state.annotations.collectFirst {
      case x: MetricIRFileAnnotation => x.value
    } map { filename =>
      val writer = new java.io.FileWriter(filename)
      val graph = new InstanceGraph(state.circuit)
      val text = state.annotations.collect {
        case x: ModuleAreaAnnotation =>
          graph.findInstancesInHierarchy(x.target.name).zipWithIndex.map { case (path, id) => ModuleAreaMetric(path.tail.map(_.name), s"${x.key}_${id}", x.namespace) }
        case x: CriticalPathAnnotation =>
          graph.findInstancesInHierarchy(x.target.name).zipWithIndex.map { case (path, id) =>
            CriticalPathMetric(path.tail.map(_.name), s"${x.key}_${id}", x.namespace)
          }
      } reduce (_ ++ _) groupBy (_.namespace) foreach { case (namespace, annos) =>
        writer.write(s"${namespace}:\n" + annos.map(_.toMetricIR(2)).mkString(""))
      }
      writer.close()
    }
    // Return the input state (we don't change anything here)
    state
  }
}
