package aoplib.floorplan
import chisel3._
import chisel3.aop.Aspect
import chisel3.core.Reset
import chisel3.experimental.{RunFirrtlTransform}
import firrtl.annotations._
import firrtl.options.Unserializable
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, LowForm, MidForm, RenameMap, Transform}

import scala.reflect.runtime.universe.TypeTag

case class MemberTracker(name: String, targets: Seq[IsMember], finalSelection: Seq[IsMember] => Option[IsMember]) extends Annotation with Unserializable {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newMembers = targets.flatMap { m: IsMember =>
      renames.get(m) match {
        case Some(seq) => seq
        case None => Seq(m)
      }
    }
    Seq(this.copy(targets = newMembers))
  }
}

/*
case class FloorplanAspect[T <: RawModule](name: String, dir: String, buildFloorplan: T => LayoutBase)
                                          (implicit tTag: TypeTag[T]) extends Aspect[T] {
  def collectTrackers(layout: LayoutBase): Seq[MemberTracker] = {
    def visit(layout: LayoutBase): Seq[MemberTracker] = {
      val trackers = if(layout.properties.get(TargetKey).nonEmpty) {
        val (target, finalMapping) = layout.get(TargetKey).asInstanceOf[(IsMember, Seq[IsMember] => Option[IsMember])]
        Seq(MemberTracker(layout.name, Seq(target), finalMapping))
      } else Nil
      layout match {
        case arr: ArrayLayout => trackers ++ arr.elements.flatMap(visit)
        case other => trackers
      }
    }
    visit(layout)
  }
  override def toAnnotation(top: T): AnnotationSeq = {
    val layout = buildFloorplan(top)
    val trackers = collectTrackers(layout)
    Seq(FloorplanInfo(layout, dir, name), RunFirrtlTransformAnnotation(new FloorplanTransform())) ++ trackers
  }
}
*/

case class FloorplanAspectNew[T <: RawModule](name: String, dir: String, buildFloorplan: T => AnnotationSeq)
                                          (implicit tTag: TypeTag[T]) extends Aspect[T] {
  override def toAnnotation(top: T): AnnotationSeq = {
    buildFloorplan(top) ++ Seq(
      RunFirrtlTransformAnnotation(new barstools.floorplan.firrtl.GenerateFloorplanIRPass()),
      barstools.floorplan.firrtl.FloorplanIRFileAnnotation("floorplan.ir")
    )
  }
}
