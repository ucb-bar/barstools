package aoplib.custom

import chisel3.aop.Aspect
import chisel3.core.RawModule
import firrtl.{AnnotationSeq, Transform}

import scala.reflect.runtime.universe.TypeTag

case class AnnotatingAspect[T <: RawModule](annotateSignals: T => AnnotationSeq)
                                           (implicit tTag: TypeTag[T]) extends Aspect[T] {
  override def toAnnotation(top: T): AnnotationSeq = annotateSignals(top)
}
