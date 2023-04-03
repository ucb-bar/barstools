package barstools.tapeout.transforms


import firrtl.Mappers._
import firrtl._
import firrtl.annotations.{CircuitTarget, ModuleTarget, SingleTargetAnnotation}
import firrtl.ir._
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.options.{Dependency}

import scala.collection.mutable



/*
circuit      -> 
|- m0          
|  - s0 (m1)
|  - s1
|- m1
|  - s0
|  - s1 (m2)
|- m2 (Tile)
|  - s0
|  - s1
|  - s2
*/

class SplitTileTransforms extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[TransformDependency] = Forms.LowForm :+
    Dependency[ExtraLowTransforms]
  override def optionalPrerequisites:  Seq[TransformDependency] = Forms.LowFormOptimized
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Forms.LowEmitters
  override def invalidates(a: Transform): Boolean = false

  private var tileNames = mutable.Set[String]()
  private var tilePorts = mutable.Map[String, Seq[Port]]()
// private var tileAsSubmodulePorts = mutable.Map[String, Seq[Port]]
  private var tileAsDirectSubmodule = mutable.Map[String, String]() // up -> tile
  private var tileAsSubmodule = mutable.Map[String, String]() // up -> down
  private var curModuleName = ""


  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    println(s"circuit.main ${circuit.main}")
    println(s"circuit.info.serialize ${circuit.info.serialize}")

    circuit.map(walkModules(_))

    tileNames.foreach { tn =>
      tilePorts(tn).foreach(p => println(s"TileName ${tn} : ${p.name}"))
    }
    tileAsDirectSubmodule.foreach { case(p, t) =>
      println(s"tileAsDirectSubmodule ${p}->${t}")
    }
    tileAsSubmodule.foreach { case(gp, p) =>
      println(s"tileAsSubmodule ${gp}->${p}")
    }

    circuit.map(punchOutTilePorts(_))


    state
  }

  def walkModules(m: DefModule): DefModule = {
    curModuleName = m.name
    println(m.name)

    if ((m.name contains "RocketTile") || (m.name contains "BoomTile")) {
      tileNames += m.name
      tilePorts(m.name) = m.ports
    }
    m.map(walkStatements)
  }

  def walkStatements(s: Statement): Statement = {
    val visited = s.map(walkStatements)
    visited match {
      case DefInstance(_, _, modName, _) =>
        if (tileNames.contains(modName)) {
          tileAsDirectSubmodule(curModuleName) = modName
        } else {
          val p = tileAsDirectSubmodule.getOrElse(modName, None)
          p match {
            case _: String => tileAsSubmodule(curModuleName) = modName
            case _ =>
              val gp = tileAsSubmodule.getOrElse(modName, None)
              gp match {
                case _: String => tileAsSubmodule(curModuleName) = modName
                case _ =>
              }
          }
        }
        s
      case notinstance => notinstance
    }
  }

  def punchOutTilePorts(m: DefModule): DefModule = {
    val hasTileAsChild = tileAsDirectSubmodule.getOrElse(m.name, None)
    hasTileAsChild match {
      case tname: String =>
        println("ADDING PORTS")
        val tports = tilePorts(tname)
        m.ports = m.ports ++ tports
      case _ =>
    }

  }
}
