// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan.FloorplanState

abstract class Pass {

  def execute(state: FloorplanState): FloorplanState

}

class SidebandAnnotationPass {

  def execute(state: FloorplanState): FloorplanState = ???

}

class MacroLayoutsPass {

  def execute(state: FloorplanState): FloorplanState = ???

}


class BottomUpPropagationPass {

  def execute(state: FloorplanState): FloorplanState = ???

}

class TopDownPropagationPass {

  def execute(state: FloorplanState): FloorplanState = ???

}


