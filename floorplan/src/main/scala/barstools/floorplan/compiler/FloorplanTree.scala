// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.mutable.{ArrayBuffer, HashMap}
import barstools.floorplan._

class FloorplanTree(val state: FloorplanState, val topMod: String) {

  val allNodes = new HashMap[String, Node]()

  class Node(val parent: Option[Node], initialRecord: FloorplanRecord) {
    val children = new ArrayBuffer[Node]()

    // TODO this might be dangerous
    private var _record = initialRecord

    def record = _record

    def addChildRecord(cRecord: FloorplanRecord): Node = {
      val n = new Node(Some(this), cRecord)
      children += n
      allNodes += (cRecord.element.name -> n)
      n
    }

    def replace(r: FloorplanRecord) { _record = r }
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
  def getNode(s: String): Node = allNodes(s)

  // These are only used by the constructor
  private val allRecords: Map[String, FloorplanRecord] = state.records.map({ x => (x.element.name -> x) }).toMap
  private def _getRecord(s: String): FloorplanRecord = allRecords(s)

  val topRecords = state.records.flatMap({ r => r.element match {
    case e: Top => Seq(r)
    case _ => Seq()
  }})
  assert(topRecords.length == 1, "Must be exactly one Top record")
  val topRecord = topRecords(0)

  private def dfs(parent: Option[Node], r: FloorplanRecord): Node = {
    r.element match {
      case e: Top =>
        assert(!parent.isDefined, "Cannot have multiple tops")
        val n = new Node(None, r)
        // There's probably a better way to handle these
        e match {
          case e: ConstrainedHierarchicalTop =>
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
  def traverseMapPre(f: (Node => Option[FloorplanRecord])) { traverseMapPreHelper(topNode, f) }
  def traverseMapPost(f: (Node => Option[FloorplanRecord])) { traverseMapPostHelper(topNode, f) }

  private def traverseMapPreHelper(n: Node, f: (Node => Option[FloorplanRecord])) {
    f(n).foreach { r => n.replace(r) }
    n.children.foreach { c => traverseMapPreHelper(c, f) }
  }

  private def traverseMapPostHelper(n: Node, f: (Node => Option[FloorplanRecord])) {
    n.children.foreach { c => traverseMapPostHelper(c, f) }
    f(n).foreach { r => n.replace(r) }
  }

  def toState: FloorplanState = {
    val records = allNodes.values.map(_.record).toSeq
    val level = records.map(_.element.level).max
    FloorplanState(records, level)
  }

}
