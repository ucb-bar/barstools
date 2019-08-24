// See LICENSE for license details.

package beagleutils

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.annotations.{InstanceTarget, ModuleTarget, Annotation, SingleTargetAnnotation}
import firrtl.Mappers._
import firrtl.Utils._

import scala.collection.mutable.ArrayBuffer

case class ExtractionAnnotation(target: chisel3.core.BaseModule)
    extends chisel3.experimental.ChiselAnnotation {
  def toFirrtl = FirrtlExtractionAnnotation(target.toNamed.toTarget)
}

case class FirrtlExtractionAnnotation(target: ModuleTarget) extends
    SingleTargetAnnotation[ModuleTarget] {
  def duplicate(rt: ModuleTarget) = this.copy(target = rt)
}

case class FirrtlInstAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}
case class PromoteSubmoduleAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

class ExtractToTop extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm

  def promoteModels(state: CircuitState): CircuitState = {
    val anns = state.annotations.flatMap {
      case a @ FirrtlInstAnnotation(it) if (it.module != it.circuit) => Seq(a, PromoteSubmoduleAnnotation(it))
      case a => Seq(a)
    }
    if (anns.toSeq == state.annotations.toSeq) {
      state
    } else {
      promoteModels((new PromoteSubmodule).runTransform(state.copy(annotations = anns)))
    }
  }

  override def execute(state: CircuitState): CircuitState = {
    val xtractModuleAnnos = state.annotations.collect({case xtract: FirrtlExtractionAnnotation => xtract.target})

    val circ = state.circuit
    val addAnnos = new ArrayBuffer[Annotation]
    val xformedModules = circ.modules.map({
      case m: Module =>
        val mt = ModuleTarget(circ.main, m.name)
        def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match {
          case inst: WDefInstance =>
            if (xtractModuleAnnos.contains(ModuleTarget(circ.main, inst.module))) {
              addAnnos += FirrtlInstAnnotation(mt.instOf(inst.name, inst.module))
            }
            inst
          case s =>
            s
        }
        m.copy(body = m.body.map(onStmt))
      case m => m
    })

    val xformedState = state.copy(annotations = state.annotations ++ addAnnos)
    val prom = promoteModels(xformedState)

    val outputFile = new java.io.PrintWriter("/tools/B/abejgonza/beagle-work/beagle-chip/vlsi/prom.fir")
    outputFile.write(prom.circuit.serialize)
    outputFile.close()

    prom

  }
}
