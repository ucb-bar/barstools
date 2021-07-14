// See LICENSE for license details
package barstools.floorplan.compiler

import firrtl.annotations.TargetToken.{Instance, OfModule}

import barstools.floorplan._
import scala.collection.Map

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
  def execute(state: FloorplanState): FloorplanState = {
    val newRecords = state.records.map({ record =>
      record.element match {
        case e: MemElement =>
          // TODO fail gracefully if key does not exist
          instMap(OfModule(record.memExt.get)).map { case (inst: Instance, ofMod: OfModule) =>
            val element = AbstractMacro(ofMod.value)
            FloorplanElementRecord(
              root = record.root,
              inst = record.inst.map(_ + "/" + inst.value),
              element = element,
              memExt = None
            )
          }
        case _ => Seq(record)
      }
    }).flatten
    state.copy(records = newRecords)
  }
}

