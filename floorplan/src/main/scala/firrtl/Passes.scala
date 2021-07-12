// See LICENSE for license details
package barstools.floorplan.firrtl

import barstools.floorplan.{FloorplanSerialization, FloorplanElementRecord, FloorplanState}
import firrtl.{CircuitState, Namespace, Transform, AnnotationSeq, VerilogEmitter, DependencyAPIMigration}
import firrtl.options.{Dependency, RegisteredTransform, ShellOption}
import firrtl.analyses.{InstanceKeyGraph}
import firrtl.annotations.{InstanceTarget}

// NOTE: If you rename/add this transform, don't forget to update META-INF
// See the @note in the RegisteredTransform documentation
class GenerateFloorplanIRPass extends Transform with RegisteredTransform with DependencyAPIMigration {

  override def prerequisites = Seq(Dependency[VerilogEmitter])
  override def optionalPrerequisites = Nil
  override def dependents = Nil
  override def invalidates(xform: Transform) = false

  val options = Seq(
    new ShellOption[String](
      longOption = "floorplan-ir-file",
      toAnnotationSeq = (a: String) => Seq(FloorplanIRFileAnnotation(a)),
      helpText = s"Set the floorplan IR file name"
    )
  )

  private def getInstancePathsFromGraph(graph: InstanceKeyGraph, cktName: String, name: String): Seq[String] = {
    if (cktName == name) {
      Seq("")
    } else {
      graph.findInstancesInHierarchy(name).map(_.map(_.name).mkString(".") + ".")
    }
  }


  def execute(state: CircuitState): CircuitState = {

    def getInstancePath(t: InstanceTarget): String = t.asPath.toList.map(_._1.value).mkString(".")
    def newRecord(path: String, anno: FloorplanAnnotation) = FloorplanElementRecord(path, FloorplanSerialization.deserialize(anno.fpir))

    val list = state.annotations.collect({
      case x: NoReferenceFloorplanAnnotation =>
        newRecord(getInstancePath(x.target), x)
      case x: InstanceFloorplanAnnotation if x.targets.flatten.length > 0 =>
        newRecord(getInstancePath(x.targets(0)(0).asInstanceOf[InstanceTarget]), x)
    })

    val filename = state.annotations.collectFirst({
      case x: FloorplanIRFileAnnotation => x.value
    }).getOrElse {
      val opt = options.head.longOption
      throw new Exception(s"Did not specify a filename for GenerateFloorplanIRPass. Please provide a FloorplanIRFileAnnotation or use the --${opt} option.")
    }
    val writer = new java.io.FileWriter(filename)
    writer.write(FloorplanState.serialize(list))
    writer.close()

    state
  }
}

