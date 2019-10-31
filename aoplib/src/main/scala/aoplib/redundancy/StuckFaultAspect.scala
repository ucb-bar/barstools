package aoplib.redundancy

import aoplib.AnnotationHelpers
import chisel3.Data
import chisel3.aop.Aspect
import chisel3.core.{RawModule}
import chisel3.experimental.{RunFirrtlTransform}
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, MidForm, RenameMap, ResolvedAnnotationPaths, Transform, WRef}
import firrtl.annotations.{Annotation, ReferenceTarget}

import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag

case class StuckFaultAspect[DUT <: RawModule, M <: RawModule](selectSignals: DUT => Seq[Data])
                                                             (implicit dutTag: TypeTag[DUT]) extends Aspect[DUT] {
  override def toAnnotation(dut: DUT): AnnotationSeq = {
    Seq(FaultyRegisters(selectSignals(dut).map(_.toTarget)))
  }
}


case class FaultyRegisters(regs: Seq[ReferenceTarget]) extends Annotation with RunFirrtlTransform {
  override def update(renames: RenameMap): Seq[Annotation] = {
    Seq(FaultyRegisters(AnnotationHelpers.renameMany(regs, renames)))
  }
  override def transformClass: Class[_ <: Transform] = classOf[StuckFaultTransform]
  override def toFirrtl  = this
}

class StuckFaultTransform extends Transform with ResolvedAnnotationPaths {
  import firrtl.ir._
  import firrtl.Mappers._
  override def inputForm: CircuitForm = MidForm
  override def outputForm: CircuitForm = MidForm

  override val annotationClasses: Traversable[Class[_]] = Seq(classOf[FaultyRegisters])

  override def execute(state: CircuitState): CircuitState = {
    val faultyRegs = state.annotations.flatMap {
      case r: FaultyRegisters => r.regs
      case other => Nil
    }

    val regModuleMap = mutable.HashMap[String, Set[String]]()
    faultyRegs.foreach { rt =>
      assert(rt.path == Nil && rt.component == Nil,
        s"Cannot have a register reference target with a component or a path: $rt")
      regModuleMap(rt.module) = regModuleMap.getOrElse(rt.module, Set.empty[String]) + rt.ref
    }

    val newModules = state.circuit.modules.map {
      case m: Module if regModuleMap.contains(m.name) =>
        val ret = m map makeFaulty(regModuleMap(m.name))// map addMuxing(regMap)
        ret
      case other => other
    }

    val newState = state.copy(
      circuit = state.circuit.copy(modules = newModules),
      annotations = state.annotations.filterNot(_.isInstanceOf[FaultyRegisters])
    )

    newState
  }

  private def makeFaulty(regs: Set[String])(s: Statement): Statement = {
    s match {
      case con@Connect(_, w@WRef(reg, tpe, _, _), expr) if regs.contains(reg) =>
        tpe match {
          case _: UIntType => con.copy(expr = UIntLiteral(0))
          case _: SIntType => con.copy(expr = SIntLiteral(0))
        }
      case other => other map makeFaulty(regs)
    }
  }
}
