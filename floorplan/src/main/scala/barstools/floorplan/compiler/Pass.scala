// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

object Pass {

  def all(opts: FloorplanOptions): Seq[Pass] = {
    val instMap = MemInstMap.fromFiles(opts.memInstMapFiles)
    val sbAnnos = SidebandAnnotationMap.fromFiles(opts.sbAnnoFiles)
    Seq(
      new TransformMemsPass(instMap),
      new SidebandAnnotationPass(sbAnnos),
      new ReplaceHierarchicalPass(opts.topMod),
      new TopDownPropagationPass(opts.topMod),
      new BottomUpPropagationPass(opts.topMod),
      new ResolveConstraintsPass,
      new CalculatePlacementsPass
    )
  }
}

abstract class Pass {
  def execute(state: FloorplanState): FloorplanState
}

class ResolveConstraintsPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}

class CalculatePlacementsPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}
