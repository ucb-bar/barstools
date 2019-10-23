// See LICENSE for license details
package barstools.floorplan.chisel

import barstools.floorplan.firrtl.{FloorplanModuleAnnotation}
import barstools.floorplan.{Element}
import chisel3.experimental.{annotate, ChiselAnnotation, RawModule}


object Floorplan {

  def apply[T <: RawModule, U <: Element](m: T, fpElement: U): Unit = {
    annotate(new ChiselAnnotation { def toFirrtl: FloorplanModuleAnnotation = FloorplanModuleAnnotation(m.toNamed.toTarget, fpElement.serialize) })
  }

}
