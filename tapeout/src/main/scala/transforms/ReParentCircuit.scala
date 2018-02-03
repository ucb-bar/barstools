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
    /*
    val modMapRename = RenameMap(ret.circuit.modules.flatMap {
      case Module(_, mName, ports, _) =>
        ports.map { case p => ComponentName(p.name, ModuleName(mName, oldCName)) -> List(ComponentName(p.name, ModuleName(mName, newCName))) }
      case _ => List()
    }.toMap)
    */
   val fixedAnnos = Some(AnnotationMap(ret.annotations.map { _.annotations.map { an => an.copy(target = an.target match {
        case ComponentName(n, ModuleName(m, _)) => ComponentName(n, ModuleName(m, newCName))
        case ModuleName(m, _) => ModuleName(m, newCName)
        case CircuitName(_) => newCName
        case named => named
        })}}.get ))
    state.copy(circuit = ret.circuit, form = outputForm, annotations = fixedAnnos)//renames = Some(modMapRename))
    //CircuitState(ret.circuit, outputForm, ret.annotations, Some(modMapRename))
  }
}
