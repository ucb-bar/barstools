// See LICENSE for license details
package barstools.floorplan.compiler

import firrtl.annotations.TargetToken.{Instance, OfModule}

import barstools.floorplan._
import scala.collection.{Map, Set}
import scala.collection.mutable.{HashMap, HashSet}


object FloorplanPasses {

  // TODO make this customizable
  def apply(opts: FloorplanOptions): Seq[Pass] = {
    val instMap = MemInstMap.fromFiles(opts.memInstMapFiles)
    val sbAnnos = Seq() // TODO
    Seq(
      new SidebandAnnotationPass(sbAnnos),
      new TransformMemsPass(instMap),
      new TopDownPropagationPass,
      new BottomUpPropagationPass,
      new ResolveConstraintsPass
    )
  }
}

abstract class Pass {
  def execute(state: FloorplanState): FloorplanState
}

class SidebandAnnotationPass(annos: Seq[SidebandAnnotation]) extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}

class TopDownPropagationPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}

class BottomUpPropagationPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}

class ResolveConstraintsPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}

class TransformMemsPass(instMap: Map[OfModule, Seq[(Instance, OfModule)]]) extends Pass {

  val nameSet = new HashSet[String]()
  val renameMap = new HashMap[String, Set[String]]()

  def getUniqueName(suggestion: String): String = {
    // This is ugly and probably slow
    var i = 0
    var tempName = suggestion + s"_${i}"
    while (nameSet.contains(tempName)) {
      tempName = suggestion + s"_${i}"
      i += 1
    }
    nameSet.add(tempName)
    tempName
  }

  def execute(state: FloorplanState): FloorplanState = {
    nameSet ++= state.records.map(_.element.name).toSet
    // Need to update in two passes
    val newRecords = state.records.flatMap({ record =>
      record.element match {
        case e: MemElement =>
          // TODO fail gracefully if key does not exist
          instMap(OfModule(record.memExt.get)).map { case (inst: Instance, ofMod: OfModule) =>
            nameSet.remove(e.name)
            val element = AbstractMacro(getUniqueName(e.name + "_" + ofMod.value))
            renameMap.update(e.name, renameMap.getOrElse(e.name, Set()) ++ Set(element.name))
            FloorplanElementRecord(
              root = record.root,
              inst = record.inst.map(_ + "/" + inst.value),
              element = element,
              memExt = None
            )
          }
        case _ => Seq(record)
      }
    }).map({ record =>
      record.element match {
        case e: MemElementArray =>
          val element = MemMacroArray(
            name = e.name,
            elements = e.elements.filter(_.isDefined).flatMap(x => renameMap(x.get)).map(x => Some(x)),
            width = e.width,
            height = e.height,
            area = e.area,
            aspectRatio = e.aspectRatio
          )
          FloorplanElementRecord(
            root = record.root,
            inst = record.inst, // should always be None
            element = element,
            memExt = None
          )
        case _ => record
      }
    })
    state.copy(records = newRecords)
  }
}

