package barstools.tapeout.transforms


import firrtl.Mappers._
import firrtl._
import firrtl.annotations.{CircuitTarget, ModuleTarget, SingleTargetAnnotation}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.options.{Dependency}

import scala.collection.mutable



class RemoveTileTransforms extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm
  override def optionalPrerequisites:  Seq[TransformDependency] = Forms.LowFormOptimized
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Forms.LowEmitters
  override def invalidates(a: Transform): Boolean = false


  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    println(s"circuit.main ${circuit.main}")
    println(s"circuit.info.serialize ${circuit.info.serialize}")

    val newCircuit = circuit.map(walkModules(_))
    state.copy(circuit = newCircuit)
  }

  def walkModules(m: DefModule): DefModule = {
    val newModule = if ((m.name contains "RocketTile") || (m.name contains "BoomTile")) {
      Module(info=m.info, name=m.name + "PortOnly", ports=m.ports, body=EmptyStmt)
    } else {
      m
    }
    newModule
  }
}
