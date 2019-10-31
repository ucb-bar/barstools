// See LICENSE for license details
package barstools.floorplan.chisel

import barstools.floorplan.firrtl.{FloorplanModuleAnnotation, GenerateFloorplanIRPass}
import barstools.floorplan.{Element, Group, Primitive}
import chisel3.{RawModule}
import chisel3.experimental.{annotate, ChiselAnnotation}

import firrtl.stage.RunFirrtlTransformAnnotation


private object FloorplanAnnotation {

  private var annotatedPass = false

  def apply[T <: RawModule, U <: Primitive](m: T, fpElement: U): Unit = {
    annotate(new ChiselAnnotation { def toFirrtl: FloorplanModuleAnnotation = FloorplanModuleAnnotation(m.toTarget, fpElement.serialize) })

    // Only add the RunFirrtlTransformAnnotation once
    if (!annotatedPass) {
      annotate(new ChiselAnnotation {
        def toFirrtl: RunFirrtlTransformAnnotation = new RunFirrtlTransformAnnotation(new GenerateFloorplanIRPass())
      })
      annotatedPass = true
    }
  }

}

