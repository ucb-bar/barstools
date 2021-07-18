// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.mutable.{HashMap}
import barstools.floorplan._

class ReplaceHierarchicalPass(val topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {

    // TODO this pass is for resolving a design to a flattened floorplan. To
    // support hierarchical PNR this will need to change

    // Find HierarchicalTop records, then stitch them together
    val topMaps = (state.records.flatMap(r => r.element match {
      case e: ConstrainedHierarchicalTop => Some((r.fullPath -> r))
      case e: PlacedHierarchicalTop => Some((r.fullPath -> r))
      case _ => None
    })).toMap

    // TODO we need to also change the root path for all elements underneath these
    val newRecords = state.records.flatMap({r => r.element match {
      case e: HierarchicalBarrier =>
        assert(topMaps.contains(r.fullPath), s"All HierarchicalBarriers must have a corresponding HierarchicalTop: ${r.fullPath} is missing")
        val newE = (topMaps(r.fullPath).element match {
          case t: ConstrainedHierarchicalTop =>
            ConstrainedElasticGrid(
              name = t.name,
              parent = e.parent,
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(t.topGroup)),
              width = t.width,
              height = t.height,
              area = t.area,
              aspectRatio = t.aspectRatio
            )
          case t: PlacedHierarchicalTop =>
            SizedGrid(
              name = t.name,
              parent = e.parent,
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(t.topGroup)),
              widths = Seq(t.width),
              heights = Seq(t.height)
            )
          case _ => ???
        })
        Seq(FloorplanElementRecord(
          root = r.root, // TODO clean up path
          inst = r.inst,
          ofModule = r.ofModule,
          element = newE
        ))
      case e: ConstrainedHierarchicalTop if r.ofModule != Some(topMod) => Seq()
      case e: PlacedHierarchicalTop if r.ofModule != Some(topMod) => Seq()
      case e => Seq(r)
    }})
    state.copy(records = newRecords, level = 2) // TODO recalculate level
  }
}
