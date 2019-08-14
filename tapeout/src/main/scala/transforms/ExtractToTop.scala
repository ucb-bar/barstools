// See LICENSE for license details.

package barstools.tapeout.transforms

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.annotations.{InstanceTarget, ModuleTarget, Annotation, SingleTargetAnnotation}
import firrtl.Mappers._
import firrtl.Utils._

import scala.collection.mutable.ArrayBuffer

case class FirrtlInstAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}
case class PromoteSubmoduleAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

class ExtractToTopModel extends Transform {
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
      println("Run and redo promotion")
      var sta: CircuitState = state
      try {
        sta = promoteModels((new PromoteSubmodule).runTransform(state.copy(annotations = anns)))
      } catch {
        case e: Throwable => e.printStackTrace
      }
      sta
    }
  }

  override def execute(state: CircuitState): CircuitState = {
    val xtractModuleAnnos = state.annotations.collect({case xtract: boom.exu.FirrtlExtractionAnnotation => xtract.target})
    println("xtractModuleAnnos: " + xtractModuleAnnos)

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

    println("New Annotations: " + addAnnos)
    val xformedState = state.copy(annotations = state.annotations ++ addAnnos)

    println("Start promotion")
    println("---PRINTING FROM EXTRACTTOTOP")
    xformedState.circuit.modules.map(x => println(x.name))
    println("---END PRINTING FROM EXTRACTTOTOP")
    promoteModels(xformedState)
  }
}
