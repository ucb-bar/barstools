/*
package aoplib.coverage

import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.{CircuitForm, CircuitState, LowForm, ResolvedAnnotationPaths, Transform}

case class SimulationFinished(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = SimulationFinished(n)
}


class CoverTransform extends Transform with ResolvedAnnotationPaths {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  override val annotationClasses: Traversable[Class[_]] = List(classOf[SimulationFinished])

  override def execute(state: CircuitState): CircuitState = {
    val simFinishedSeq = state.annotations.collect {
      case c: SimulationFinished => c
    }

    assert(simFinishedSeq.size == simFinishedSeq.toSet.size,
      s"There can only be one finished simulation signal, found: ${simFinishedSeq.map(_.target.serialize)}")

    val done = simFinishedSeq.toSet.head.target

    val circuit = state.circuit
    done match {
      case ReferenceTarget(cir, mod, Nil, ref, Nil) =>
        assert(circuit.main == cir, s"Simulation Signal circuit does not match given circuit: $cir != ${circuit.main}")
        assert(circuit.modules.map(_.name).contains(mod), s"Simulation Signal module $mod is not found in circuit $cir")
        val module = circuit.modules.collectFirst{
          case m if m.name == mod => m
        }.get


      case other => sys.error(s"Malformed simulation finished signal target: $other")
    }

    state
  }

}
*/
