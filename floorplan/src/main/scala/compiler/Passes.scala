// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan.FloorplanState

abstract class Pass {

  def execute(state: FloorplanState): FloorplanState

}

