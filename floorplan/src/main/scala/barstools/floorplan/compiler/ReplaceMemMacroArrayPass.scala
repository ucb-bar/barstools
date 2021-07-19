// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class ReplaceMemMacroArrayPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    // TODO this needs some additional options, etc.
    // For the proof-of-concept, we'll just replace them with an Array that meets whatever box constraints
    state // TODO
  }
}
