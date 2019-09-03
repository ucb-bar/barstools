// See LICENSE for license details.

package beagleutils

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.annotations._
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

class ExtractToTop extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm

  def promoteModels(state: CircuitState): CircuitState = {
    val anns = state.annotations.flatMap {
      case a @ FirrtlInstAnnotation(it) if (it.module != it.circuit) => {
        println(s"AddPromoteAnno: ${firrtl.transforms.PromoteSubmoduleAnnotation(it, "Wire2_")}")
        Seq(a, firrtl.transforms.PromoteSubmoduleAnnotation(it, "Wire2_"))
      }
      case a => Seq(a)
    }
    if (anns.toSeq == state.annotations.toSeq) {
      state
    } else {
      val lowPromCirc = (new firrtl.MiddleFirrtlToLowFirrtl).runTransform(state.copy(annotations = anns))
      println("Comp Mid->Low")
      val promCirc = (new firrtl.transforms.PromoteSubmodule).runTransform(lowPromCirc)
      println("Comp Prom")
      val resPromCirc = (new ResolveAndCheck).runTransform(promCirc)
      println("Comp Res")
      val midPromCirc = (new HighFirrtlToMiddleFirrtl).runTransform(resPromCirc)
      println("Comp High->Mid")
      promoteModels(midPromCirc)
    }
  }

  override def execute(state: CircuitState): CircuitState = {
    val xtractModuleAnnos = state.annotations.collect({case xtract: FirrtlExtractionAnnotation => xtract.target})
    println(s"ExtractAnnos: $xtractModuleAnnos")

    val circ = state.circuit
    val addAnnos = new ArrayBuffer[Annotation]
    val xformedModules = circ.modules.map({
      case m: Module =>
        val mt = ModuleTarget(circ.main, m.name)
        def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match {
          case inst: WDefInstance =>
            if (xtractModuleAnnos.contains(ModuleTarget(circ.main, inst.module))) {
              val instAnno = FirrtlInstAnnotation(mt.instOf(inst.name, inst.module))
              addAnnos += instAnno
            }
            inst
          case s =>
            s
        }
        m.copy(body = m.body.map(onStmt))
      case m => m
    })

    println(s"AddedAnnos: ${addAnnos}")
    println("StartPromote")

    var promMo = state
    for (anno <- addAnnos) {
      println(s"Running promote with $anno")
      val xformedState = promMo.copy(annotations = state.annotations ++ Seq(anno))
      promMo = promoteModels(xformedState)
    }

    promMo
  }
}
