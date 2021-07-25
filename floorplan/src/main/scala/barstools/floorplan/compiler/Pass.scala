// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

object Pass {

  def all(opts: FloorplanOptions): Seq[Pass] = {
    val instMap = MemInstMap.fromFiles(opts.memInstMapFiles)
    val sbAnnos = OutOfBandAnnotationMap.fromFiles(opts.sbAnnoFiles)
    Seq(
      new TransformMemsPass(instMap),
      new OutOfBandAnnotationPass(sbAnnos),
      new ReplaceHierarchicalPass(opts.topMod),
      new ConstraintPropagationPass(opts.topMod),
      new ReplaceMemMacroArrayPass,
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

