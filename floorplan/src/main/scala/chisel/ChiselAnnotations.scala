// See LICENSE for license details
package barstools.floorplan.chisel

import barstools.floorplan.firrtl.{FloorplanModuleAnnotation, GenerateFloorplanIRPass}
import barstools.floorplan.{Element}
import chisel3.core.{RawModule}
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.stage.RunFirrtlTransformAnnotation


object Floorplan {

  private var annotatedPass = false

  def setLayout[T <: RawModule, U <: Element](m: T, fpElement: U): Unit = {
    annotate(new ChiselAnnotation { def toFirrtl: FloorplanModuleAnnotation = FloorplanModuleAnnotation(m.toNamed.toTarget, fpElement.serialize) })

    // Only add the RunFirrtlTransformAnnotation once
    if (!annotatedPass) {
      annotate(new ChiselAnnotation {
        def toFirrtl: RunFirrtlTransformAnnotation = new RunFirrtlTransformAnnotation(new GenerateFloorplanIRPass())
      })
      annotatedPass = true
    }
  }

}

