// See LICENSE for license details
package barstools.floorplan

package object chisel {

  import chisel3.experimental.{BaseModule}

  type FloorplanFunction = PartialFunction[BaseModule, Seq[ChiselElement]]

}

