// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

object Pass {

  def all(opts: FloorplanOptions): Seq[Pass] = {
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

