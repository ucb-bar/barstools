package aoplib.coverage

import aoplib.histogram.{HistogramAspect, HistogramSignal}
import chisel3.{Bits, Bool, Data}
import chisel3.aop.injecting.{InjectingAspect, InjectingTransform}
import chisel3.aop.{Aspect, Select}
import chisel3.core.RawModule
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.util.experimental.BoringUtils
import firrtl.{AnnotationSeq, RenameMap}
import firrtl.annotations.{Annotation, IsMember}
import firrtl.options.Unserializable
import firrtl.passes.wiring.WiringTransform
import firrtl.stage.RunFirrtlTransformAnnotation

import scala.collection.MapLike
import scala.reflect.runtime.universe.TypeTag


trait CoverageOption

case object SimulatorDone extends CoverageOption

case class CoverageOptions(options: Map[CoverageOption, Any]) {
  def simDone[T <: RawModule](top: T): Bool = options(SimulatorDone).asInstanceOf[T => Bool](top)
}

case class CoverAspect[T <: RawModule](buildCoverage: T => Seq[CoverGroup],
                                       coverage: CoverageOptions)
                                      (implicit tTag: TypeTag[T]) extends Aspect[T] {

  override def toAnnotation(top: T): AnnotationSeq = {
    val coverGroups = buildCoverage(top)
    val groupMap = coverGroups.groupBy { group => group.module }
    val annoSeqs = Select.collectDeep(top) {
      case x: RawModule if groupMap.contains(x) =>
        val groups = groupMap(x).toList
        assert(groups.size == 1, "Currently only support one covergroup per module")

        val ia = InjectingAspect[T, RawModule](
          (t: T) => Seq(x),
          { m: RawModule =>
            import chisel3._
            val signals = groups.flatMap { group =>
              group.points.map { case CoverPoint(labelx, sig, bins, options) =>
                val defaults = bins.collect { case b@Bin(_, Default) => b }
                assert(defaults.size <= 1, s"Coverpoint $labelx on signal ${sig.signal.toTarget} can no more than one default bin.")
                assert(defaults.isEmpty, s"Don't support Default bin yet! Sorry :(")
                val binIntervals = bins.filter { _.category.isInstanceOf[BinRange] }.sortBy {
                  case Bin(_, BinRange(low, high)) => low
                }
                new HistogramSignal(sig.signal) {
                  override def intervals: Seq[(Option[String], Int, Int)] = binIntervals.map{
                    case Bin(n, BinRange(low, high)) => (Some(n), low.toInt, high.toInt)
                  }
                  override def label: Option[String] = Some(labelx)
                }
              }
            }
            val done = Wire(Bool())
            done := DontCare
            BoringUtils.bore(coverage.simDone(top), Seq(done))
            signals.foreach { s =>
              val tracker = Module(new CoverageTracker(s, s.signal.name, m.name))
              tracker.in := s.signal
              tracker.recording := true.B
              tracker.printCoverage := done
            }
          }
        ).toAnnotation(top)

        // Create annotation to insert histogram execution after design execution
        //val ia2 = InjectingAspect({dut: T => Seq(dut)}, { dut: T => setDone(selectSimDone(dut)) }).toAnnotation(dut)

        //ia ++ ia2 ++ Seq(RunFirrtlTransformAnnotation(new InjectingTransform), RunFirrtlTransformAnnotation(new WiringTransform))
        ia
    }
    val ret = annoSeqs.toList.foldRight(Seq[Annotation](RunFirrtlTransformAnnotation(new WiringTransform))){ case (sofar, next) => next ++ sofar }
    ret
  }
}

object SignalTracker {
  def apply(signal: Bits, selector: Seq[IsMember] => Option[IsMember] = ts => Some(ts.head)): SignalTracker = {
    SignalTracker(signal, Nil, selector, expanded = false)
  }
}

case class SignalTracker(signal: Bits, targets: Seq[IsMember], finalSelection: Seq[IsMember] => Option[IsMember], expanded: Boolean) extends Annotation with Unserializable {
  def singleUpdate(renames: RenameMap): SignalTracker = {
    val renamed = update(renames)
    assert(renamed.size == 1, "Signal Tracker should always be renamed to a single other SignalTracker")
    renamed.head
  }

  override def update(renames: RenameMap): Seq[SignalTracker] = {
    val expandedTargets = if(!expanded) {
      assert(targets.isEmpty, "If SignalTracker isn't expanded, its targets should be empty.")
      Seq(signal.toTarget)
    } else targets
    val newMembers = expandedTargets.flatMap { m: IsMember =>
      renames.get(m) match {
        case Some(seq) => seq
        case None => Seq(m)
      }
    }
    if(!expanded) Seq(this.copy(targets = newMembers, expanded=true)) else Seq(this.copy(targets = newMembers))
  }
}
