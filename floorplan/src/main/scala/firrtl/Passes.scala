// See LICENSE for license details
package barstools.floorplan.firrtl

import barstools.floorplan.FloorplanSerialization
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
    state.annotations.collectFirst({
      case x: FloorplanIRFileAnnotation => x.value
    }).headOption.map { filename =>
      val writer = new java.io.FileWriter(filename)
      val graph = new InstanceGraph(state.circuit)
      val list = state.annotations.collect {
        case x: FloorplanModuleAnnotation =>
          graph.findInstancesInHierarchy(x.target.name).
            map(_.tail.map(_.name)).
            reduce(_ ++ _).
            map((_, FloorplanSerialization.deserialize(x.fpir)))
      } reduce (_ ++ _)
      writer.write(FloorplanSerialization.serialize(list))
      writer.close()
    } getOrElse {
      val opt = options.head.longOption
      throw new Exception(s"Did not specify a filename for GenerateFloorplanIRPass. Please provide a FloorplanIRFileAnnotation or use the --${opt} option.")
    }
    state
  }
}

