// See LICENSE for license details
package barstools.floorplan.firrtl

import barstools.floorplan.{FloorplanSerialization, FloorplanElementRecord, FloorplanState}
import firrtl.{CircuitState, LowForm, Namespace, Transform, AnnotationSeq}
import firrtl.options.{RegisteredTransform, ShellOption}
import firrtl.analyses.{InstanceGraph}

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

  def execute(state: CircuitState): CircuitState = {
    // TODO don't need graph if there are no annos, which can be a speedup
    val graph = new InstanceGraph(state.circuit)
    state.annotations.collect({
      case x: FloorplanModuleAnnotation =>
        graph.findInstancesInHierarchy(x.target.name).
          map(_.tail.map(_.name)).
          reduce(_ ++ _).
          map(y => FloorplanElementRecord(Some(y), FloorplanSerialization.deserialize(x.fpir)))
    }).flatten.foreach { list =>
      val filename = state.annotations.collectFirst({
        case x: FloorplanIRFileAnnotation => x.value
      }).getOrElse {
        val opt = options.head.longOption
        throw new Exception(s"Did not specify a filename for GenerateFloorplanIRPass. Please provide a FloorplanIRFileAnnotation or use the --${opt} option.")
      }
      val writer = new java.io.FileWriter(filename)
      writer.write(FloorplanState.serialize(Seq(list)))
      writer.close()
    }
    state
  }
}

