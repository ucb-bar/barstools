// See LICENSE for license details.

package barstools.tapeout.transforms

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.annotations.{CircuitName, ModuleName, ComponentName}

// "Re-Parents" a circuit, which changes the top module to something else.
class ReParentCircuitPass(newTopName: String) extends Pass {
  def run(c: Circuit): Circuit = {
    Circuit(c.info, c.modules, newTopName)
  }
}

class ReParentCircuit(newTopName: String) extends Transform with SeqTransformBased {
  def inputForm = HighForm
  def outputForm = HighForm
  def transforms = Seq(new ReParentCircuitPass(newTopName))

  def execute(state: CircuitState): CircuitState = {
    val ret = runTransforms(state)
    val oldCName = CircuitName(state.circuit.main)
    val newCName = CircuitName(newTopName)
    val renames = RenameMap()
    renames.rename(oldCName, newCName)
    CircuitState(ret.circuit, outputForm, ret.annotations, Some(renames))
  }
}
