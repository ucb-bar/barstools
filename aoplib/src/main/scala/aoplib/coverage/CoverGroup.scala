package aoplib.coverage

import aoplib.AnnotationHelpers
import chisel3.core.RawModule
import chisel3.aop.Aspect
import chisel3.aop.injecting.{InjectStatement, InjectingAspect, InjectingTransform}
import chisel3.{Clock, Reset}
import firrtl.annotations.Annotation
import firrtl.options.Unserializable
import firrtl.{AnnotationSeq, RenameMap}

import scala.reflect.runtime.universe.TypeTag

//object CoverGroup {
//  def apply(label: String,
//            module: RawModule,
//            clock: Clock,
//            reset: Reset,
//            points: Seq[CoverPoint],
//            options: GroupOptions = GroupOptions()): CoverGroup = {
//    CoverGroup(label, module, clock, reset, points, options)
//  }
//}

case class CoverGroup (label: String,
                          module: RawModule,
                          clock: Clock,
                          reset: Reset,
                          points: Seq[CoverPoint],
                          options: GroupOptions = GroupOptions(),
                         ) extends Annotation with Unserializable {
  override def update(renames: RenameMap): Seq[CoverGroup] = {
    def updateTracker(t: SignalTracker): SignalTracker = {
      val renamed = t.update(renames)
      renamed.head
    }
    Seq(this.copy(points = points.map(_.update(renames))))
  }
}

case class GroupOptions(weight: Int = 1)

/*
weight=number, 1

If set at the covergroup syntactic level, it specifies the weight of this covergroup instance for computing the overalla
 instance coverage of the simulation.
If set at the coverpoint (or cross) syntactic level, it specifies the weight of a coverpoint (or cross) for computing
 the instance coverage of the enclosing covergroup.

goal=number, 90

Specifies the target goal for a covergroup instance or for a coverpoint or a cross of an instance.

name=string, unique name

Specifies a name for the covergroup instance.

comment=string

A comment that appears with a covergroup instance or with a coverpoint or cross of the covergroup instance

at_least=number, 1

Minimum number of times a bin needs to hit before it is declared as hit

detect_overlap=boolean, 0

When true, a warning is issued if there is an overlap between the range list (or transition list) of two bins of a
coverpoint.

auto_bin_max=number, 64

Maximum number of automatically created bins when no bins are explicitly defined for a coverpoint.

cross_num_print_missing = number, 0

Number of missing (not covered) cross product bins that must be saved to the coverage database and printed in the
coverage report.

per_instance=boolean, 0

Each instance contributes to the overall coverage information for the covergroup type. When true, coverage information
for this covergroup instance is tracked as well.
 */
