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

case class FloorplanAspect[T <: RawModule](buildFloorplan: T => AnnotationSeq)(implicit tTag: TypeTag[T]) extends Aspect[T] {
  override def toAnnotation(top: T): AnnotationSeq = {
    buildFloorplan(top) :+ RunFirrtlTransformAnnotation(new barstools.floorplan.firrtl.GenerateFloorplanIRPass())
  }
}
