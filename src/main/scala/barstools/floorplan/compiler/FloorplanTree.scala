// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.mutable.{ArrayBuffer, HashMap}
import barstools.floorplan._

// TODO this is required to give access to Node types outside FloorplanTree. Ideally we should move all that functionality into this class
// with some type-generic methods, but for now this works
sealed trait FloorplanTreeNode {

  def parent: Option[FloorplanTreeNode]
  def children: Seq[FloorplanTreeNode]
  def record: FloorplanRecord
  def addChildRecord(cRecord: FloorplanRecord): FloorplanTreeNode
  def removeChild(n: FloorplanTreeNode): Unit
  def delete(): Unit
  def replace(r: FloorplanRecord): Unit
  def reparent(p: FloorplanTreeNode): Unit

}

class FloorplanTree(val state: FloorplanState, val topMod: String) {

  val allNodes = new HashMap[String, FloorplanTreeNode]()

  class Node(val parent: Option[FloorplanTreeNode], initialRecord: FloorplanRecord) extends FloorplanTreeNode {
    val _children = new ArrayBuffer[FloorplanTreeNode]()

    // TODO this might be dangerous
    private var _record = initialRecord

    def children = _children.toSeq
    def record = _record

    def addChildRecord(cRecord: FloorplanRecord): FloorplanTreeNode = {
      val n = new Node(Some(this), cRecord)
      _children += n
      allNodes += (cRecord.element.name -> n)
      n
    }

    def removeChild(n: FloorplanTreeNode) {
      _children.remove(_children.indexOf(n))
    }

    def delete() {
      assert(_children.isEmpty) // sanity check that we aren't orphaning nodes
      parent.foreach(_.removeChild(this))
      allNodes.remove(record.element.name)
    }

    def replace(r: FloorplanRecord) { _record = r }

    def reparent(p: FloorplanTreeNode) {
      this.delete()
      p.addChildRecord(record)
    }
  }


  def getUniqueName(suggestion: String): String = {
    var i = 0
    var tmp = suggestion + s"_${i}"
    while (allNodes.keys.toSet.contains(tmp)) {
      i = i + 1
      tmp = suggestion + s"_${i}"
    }
    tmp
  }

  def getRecord(s: String): FloorplanRecord = getNode(s).record
  def getNode(s: String): FloorplanTreeNode = allNodes(s)

  // These are only used by the constructor
  private val allRecords: Map[String, FloorplanRecord] = state.records.map({ x => (x.element.name -> x) }).toMap
  private def _getRecord(s: String): FloorplanRecord = allRecords(s)

  val topRecords = state.records.flatMap({ r => r.element match {
    case e: Top => Seq(r)
    case _ => Seq()
  }})
  assert(topRecords.length == 1, "Must be exactly one Top record")
  val topRecord = topRecords(0)

  private def dfs(parent: Option[FloorplanTreeNode], r: FloorplanRecord): FloorplanTreeNode = {
    r.element match {
      case e: Top =>
        assert(!parent.isDefined, "Cannot have multiple tops")
        val n = new Node(None, r)
        // There's probably a better way to handle these
        e match {
          case e: ConstrainedHierarchicalTop =>
            dfs(Some(n), _getRecord(e.topGroup))
          case e: SizedHierarchicalTop =>
            dfs(Some(n), _getRecord(e.topGroup))
          case e: PlacedHierarchicalTop =>
            e.elements.foreach(x => dfs(Some(n), _getRecord(x)))
        }
        allNodes += (e.name -> n)
        n
      case e: Primitive =>
        assert(parent.isDefined, "Must have parent")
        parent.get.addChildRecord(r)
      case e: Group =>
        assert(parent.isDefined, "Must have parent")
        val n = parent.get.addChildRecord(r)
        e.elements.foreach(x => dfs(Some(n), _getRecord(x)))
        n
      case _ => ???
    }
  }

  val topNode = dfs(None, topRecord)

  // Traverse using DFS, passing the node to a function which expects an
  //    Option[FloorplanRecord] return
  // None = do no modify
  // Some(record) = modify node
  def traverseMapPre(f: (FloorplanTreeNode => Option[FloorplanRecord])) { traverseMapPreHelper(topNode, f) }
  def traverseMapPost(f: (FloorplanTreeNode => Option[FloorplanRecord])) { traverseMapPostHelper(topNode, f) }

  private def traverseMapPreHelper(n: FloorplanTreeNode, f: (FloorplanTreeNode => Option[FloorplanRecord])) {
    f(n).foreach { r => n.replace(r) }
    n.children.foreach { c => traverseMapPreHelper(c, f) }
  }

  private def traverseMapPostHelper(n: FloorplanTreeNode, f: (FloorplanTreeNode => Option[FloorplanRecord])) {
    n.children.foreach { c => traverseMapPostHelper(c, f) }
    f(n).foreach { r => n.replace(r) }
  }

  def toState: FloorplanState = {
    val records = allNodes.values.map(_.record).toSeq
    val level = records.map(_.element.level).max
    FloorplanState(records, level)
  }

}
