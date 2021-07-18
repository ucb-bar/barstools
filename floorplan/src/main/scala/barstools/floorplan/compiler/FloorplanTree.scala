// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.mutable.{HashMap}
import barstools.floorplan.{Element, FloorplanState, FloorplanElementRecord}

/*

class FloorplanTree(state: FloorplanState, val topMod: String) {

  assert(state.level < 3, "Cannot have Hierarchicals")

  class Node(val parent: Option[Node], val record: FloorplanElementRecord) {
    val children = new HashMap[String, Node]()
    val name = record.element.name

    protected[FloorplanTree] def addNode(node: Node) {
      children =+ (node.name -> node)
    }

    protected[FloorplanTree] def addRecord(record: FloorplanElementRecord) { addNode(new Node(Some(this), record)) }
  }


}

object FloorplanTree {
  def apply(state: FloorplanState, topMod: String) = new FloorplanTree(state, topMod)
}

*/
