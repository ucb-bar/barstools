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

    val root = topMaps(topMod).root
    val rootNameSet = HashSet(state.records.filter(_.root == root).map(_.element.name):_*)

    val nonRootPaths = topMaps.filterKeys(_ != topMod).values.map(_.root)

    def getUniqueName(suggestion: String): String = {
      var i = 0
      var tmp = suggestion + s"_${i}"
      while (rootNameSet.contains(tmp)) {
        i = i + 1
        tmp = suggestion + s"_${i}"
      }
      rootNameSet += tmp
      tmp
    }

    val renameMap = new HashMap[(String, String), String]()

    def rename(oldRoot: String, oldName: String): String = {
      if (oldRoot == rootNameSet) { return oldName }
      val tup = (oldRoot, oldName)
      if (renameMap.contains(tup)) { return renameMap(tup) }
      val newName = getUniqueName(oldName)
      renameMap += (tup -> newName)
      newName
    }

    def getRelPath(fullPath: Option[String]): Option[String] = fullPath.map { p =>
      assert(p.startsWith(root), "Full path must be in root scope")
      p.substring(root.length)
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
              name = rename(r.root, e.name),
              parent = rename(r.root, e.parent),
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(rename(tr.root, t.name))),
              width = Seq(t.width),
              height = Seq(t.height),
              area = Seq(t.area),
              aspectRatio = Seq(t.aspectRatio)
            ), ConstrainedElasticGrid(
              name = rename(tr.root, t.name),
              parent = rename(r.root, e.name),
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(rename(tr.root, t.topGroup))),
              width = Seq(t.width),
              height = Seq(t.height),
              area = Seq(t.area),
              aspectRatio = Seq(t.aspectRatio)
            ))
          case t: PlacedHierarchicalTop =>
            Seq(SizedGrid(
              name = rename(r.root, e.name),
              parent = rename(r.root, e.parent),
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(rename(tr.root, t.name))),
              widths = Seq(t.width),
              heights = Seq(t.height)
            ), SizedGrid(
              name = rename(tr.root, t.name),
              parent = rename(r.root, e.name),
              xDim = 1,
              yDim = 1,
              elements = Seq(Some(rename(tr.root, t.topGroup))),
              widths = Seq(t.width),
              heights = Seq(t.height)
            ))
          case _ => ???
        }).map(newE => FloorplanElementRecord(
            root = root,
            inst = getRelPath(r.inst.map(_ => r.fullPath)),
            ofModule = r.ofModule,
            element = newE
          ))
      case e: ConstrainedHierarchicalTop if r.ofModule != Some(topMod) => Seq()
      case e: PlacedHierarchicalTop if r.ofModule != Some(topMod) => Seq()
      case e => Seq(r.copy(
        root = root,
        inst = getRelPath(r.inst.map(_ => r.fullPath)),
        ofModule = r.ofModule,
        element = r.element.mapNames(x => rename(r.root, x))
      ))
    }})
    state.copy(records = newRecords, level = 2) // TODO recalculate level
  }
}
