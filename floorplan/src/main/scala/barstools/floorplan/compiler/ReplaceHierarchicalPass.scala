// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.mutable.{HashMap, HashSet}
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

    val scope = topMaps(topMod).scope
    val scopeNameSet = HashSet(state.records.filter(_.scope == scope).map(_.element.name):_*)

    val nonScopePaths = topMaps.filterKeys(_ != topMod).values.map(_.scope)

    def getUniqueName(suggestion: String): String = {
      var i = 0
      var tmp = suggestion + s"_${i}"
      while (scopeNameSet.contains(tmp)) {
        i = i + 1
        tmp = suggestion + s"_${i}"
      }
      scopeNameSet += tmp
      tmp
    }

    val renameMap = new HashMap[(String, String), String]()

    def rename(oldScope: String, oldName: String): String = {
      if (oldScope == scopeNameSet) { return oldName }
      val tup = (oldScope, oldName)
      if (renameMap.contains(tup)) { return renameMap(tup) }
      val newName = getUniqueName(oldName)
      renameMap += (tup -> newName)
      newName
    }

    def getRelPath(fullPath: Option[String]): Option[String] = fullPath.map { p =>
      assert(p.startsWith(scope), "Full path must be in scope scope")
      p.substring(scope.length)
    }

    val newRecords = state.records.flatMap({r => r.element match {
      case e: HierarchicalBarrier =>
        assert(topMaps.contains(r.fullPath), s"All HierarchicalBarriers must have a corresponding HierarchicalTop: ${r.fullPath} is missing")
        val tr = topMaps(r.fullPath)
        (tr.element match {
          case t: ConstrainedHierarchicalTop =>
            // We replace with two elements (one to replace each name); they'll get optimized away later
            // Parent is first (replaces e), child is after (replaces t)
            Seq(ConstrainedElasticGrid(
              name = rename(r.scope, e.name),
              parent = rename(r.scope, e.parent),
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(rename(tr.scope, t.name))),
              width = t.width,
              height = t.height,
              area = t.area,
              aspectRatio = t.aspectRatio
            ), ConstrainedElasticGrid(
              name = rename(tr.scope, t.name),
              parent = rename(r.scope, e.name),
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(rename(tr.scope, t.topGroup))),
              width = t.width,
              height = t.height,
              area = t.area,
              aspectRatio = t.aspectRatio
            ))
          case t: PlacedHierarchicalTop => ??? // TODO not supported yet
          case _ => ???
        }).map(newE => FloorplanRecord(
            scope = scope,
            inst = getRelPath(r.inst.map(_ => r.fullPath)),
            ofModule = r.ofModule,
            element = newE
          ))
      case e: ConstrainedHierarchicalTop if r.ofModule != Some(topMod) => Seq()
      case e: PlacedHierarchicalTop if r.ofModule != Some(topMod) => Seq()
      case e => Seq(r.copy(
        scope = scope,
        inst = getRelPath(r.inst.map(_ => r.fullPath)),
        ofModule = r.ofModule,
        element = r.element.mapNames(x => rename(r.scope, x))
      ))
    }})
    state.copy(records = newRecords, level = 2) // TODO recalculate level
  }
}
