// See LICENSE for license details
package barstools.floorplan.chisel

import barstools.floorplan.firrtl.{FloorplanAnnotation}

import chisel3.{RawModule}
import chisel3.aop.{Aspect, Select}
import chisel3.experimental.{BaseModule}
import firrtl.{AnnotationSeq}

abstract class FloorplanAspect extends Aspect[RawModule] {

  def floorplans: PartialFunction[BaseModule, Seq[ChiselElement]]

  final override def toAnnotation(top: RawModule): AnnotationSeq = {
    // TODO we run this for each FloorplanAspect, which is inefficient. We should run Select.collectDeep only once and combine the partial functions somehow
    AnnotationSeq(Select.collectDeep(top)(floorplans).toList.flatten.flatMap(_.getAnnotations()))
  }
}

