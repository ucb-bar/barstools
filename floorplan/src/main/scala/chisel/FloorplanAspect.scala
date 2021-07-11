// See LICENSE for license details
package barstools.floorplan.chisel

import chisel3.{RawModule}
import chisel3.aop.{Aspect, Select}
import firrtl.{AnnotationSeq}

abstract class FloorplanAspect[T <: RawModule](floorplans: FloorplanFunction) extends Aspect[T] {

  // Guarantee only one FloorplanAspect will be created
  FloorplanAspect.setHasRun()

  final override def toAnnotation(top: T): AnnotationSeq = {
    AnnotationSeq(Select.collectDeep(top)(floorplans).toList.flatten.flatMap(_.getAnnotations()))
  }
}

object FloorplanAspect {
  private var hasRun = false

  private[chisel] def setHasRun() = {
    if (hasRun) {
      throw new Exception("Cannot run FloorplanAspect more than once; combine partial functions into a single FloorplanAspect instead.")
    }
    hasRun = true
  }
}
