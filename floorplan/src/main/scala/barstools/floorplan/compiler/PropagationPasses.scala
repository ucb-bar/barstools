// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class TopDownPropagationPass(val topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}

class BottomUpPropagationPass(val topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = state // TODO
}
