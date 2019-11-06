// See LICENSE for license details
package barstools.floorplan.firrtl

import barstools.floorplan.{FloorplanSerialization, FloorplanElementRecord, FloorplanState}
import firrtl.{CircuitState, LowForm, Namespace, Transform, AnnotationSeq}
import firrtl.options.{RegisteredTransform, ShellOption}
import firrtl.analyses.{InstanceGraph}
import firrtl.annotations.{InstanceTarget, ModuleTarget}

// NOTE: If you rename/add this transform, don't forget to update META-INF
// See the @note in the RegisteredTransform documentation
class GenerateFloorplanIRPass extends Transform with RegisteredTransform {
  def inputForm = LowForm
  def outputForm = LowForm

  val options = Seq(
    new ShellOption[String](
      longOption = "floorplan-ir-file",
      toAnnotationSeq = (a: String) => Seq(FloorplanIRFileAnnotation(a)),
      helpText = s"Set the floorplan IR file name"
    )
  )


  private def getInstancePathsFromGraph(graph: InstanceGraph, cktName: String, name: String): Seq[String] = {
    if (cktName == name) {
      return Seq("")
    } else {
      return graph.findInstancesInHierarchy(name).map(_.map(_.name).mkString(".") + ".")
    }
  }


  def execute(state: CircuitState): CircuitState = {
    // TODO don't need graph if there are no annos, which can be a speedup
    val graph = new InstanceGraph(state.circuit)

    def getPaths(name: String) = getInstancePathsFromGraph(graph, state.circuit.main, name)

    def newRecord(path: String, anno: FloorplanAnnotation) = FloorplanElementRecord(path, FloorplanSerialization.deserialize(anno.fpir))

    val list = state.annotations.collect({
      case x: FloorplanInstanceAnnotation => {
        val t = x.target
        getPaths(t.module).map(_ + (t.path.toList.map(_._1.value) :+ t.instance mkString ".")).map(newRecord(_, x))
      }
      case x: FloorplanModuleAnnotation => getPaths(x.target.name).map(newRecord(_, x))
    }).flatten

    if (list.nonEmpty) {
      val filename = state.annotations.collectFirst({
        case x: FloorplanIRFileAnnotation => x.value
      }).getOrElse {
        val opt = options.head.longOption
        throw new Exception(s"Did not specify a filename for GenerateFloorplanIRPass. Please provide a FloorplanIRFileAnnotation or use the --${opt} option.")
      }
      val writer = new java.io.FileWriter(filename)
      writer.write(FloorplanState.serialize(list))
      writer.close()
    }
    state
  }
}

