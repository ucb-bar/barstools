// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.mutable.{HashMap}
import barstools.floorplan.{Element, FloorplanState, FloorplanElementRecord}
/*

// TODO all of this is out of date

class FloorplanTree(state: FloorplanState) {

  class Node(val parent: Option[Node], val name: String) {
    val children = new HashMap[String, Node]()
    val elements = new HashMap[String, Element]()

    def addElement(path: Seq[String], tag: String, element: Element): Unit = {
      if (path.isEmpty) {
        if (elements.contains(tag)) {
          throw new Exception(s"Duplicate tag ${tag} at floorplan path ${getPath}")
        } else {
          elements.update(tag, element)
        }
      } else {
        val child = children.getOrElseUpdate(path.head, new Node(Some(this), path.head))
        child.addElement(path.tail, tag, element)
      }
    }

    def getPathHelper(sub: String): String = parent.map(_.getPathHelper(s"${name}.${sub}")).getOrElse(sub)

    def getPath: String = getPathHelper(name)

    def getNode(path: Seq[String]): Node = {
      if (path.isEmpty) {
        this
      } else {
        children.get(path.head).map(_.getNode(path.tail)).getOrElse {
          throw new Exception(s"Path ${getPath}.${path} does not exist in tree")
        }
      }
    }

    def getElement(tag: String): Element = {
      elements.get(tag).getOrElse { throw new Exception(s"Tag ${tag} does not exist in node ${getPath}") }
    }

    def getElement(path: Seq[String], tag: String): Element = (if (path.isEmpty) this else getNode(path)).getElement(tag)

    def getElements: Seq[(String, Element)] = {
      elements.toSeq ++ (children.mapValues(_.getElements).toSeq.map { case (name, elts) =>
        elts.map { case (path, elt) =>
          val newpath = if (name == "") path else s"${name}.${path}"
          (newpath, elt)
        }
      }).flatten
    }

    def mapElements(f: (Element) => Element): Seq[(String, Element)] = {
      ???
    }
  }

  val root = new Node(None, "")

  private def parsePathAndTag(str: String): (Seq[String], String) = {
    val split = str.split("#", -1)
    assert(split.size == 2, s"Malformed floorplan record path: ${str}")
    val path = split(0).split(".")
    val tag = split(1)
    (path, tag)
  }

  state.records.foreach { record =>
    val (path, tag) = parsePathAndTag(record.root)
    root.addElement(path, tag, record.element)
  }

  def getNode(path: Seq[String]): Node = root.getNode(path)

  def getNode(str: String): Node = {
    if (str.contains("#")) throw new Exception(s"getNode cannot be called on an element path (containing #): ${str}")
    getNode(str.split("."))
  }

  def getElement(str: String): Element = {
    val (path, tag) = parsePathAndTag(str)
    root.getElement(path, tag)
  }

  def toState: FloorplanState = {
    val records = root.getElements.map { case (path, elt) => FloorplanElementRecord(path, None, elt) } // TODO this is broken but unused right now
    FloorplanState(records, records.map(_.element.level).max)
  }

  //def traverse(

}

object FloorplanTree {

  def apply(state: FloorplanState) = new FloorplanTree(state)

}
*/
