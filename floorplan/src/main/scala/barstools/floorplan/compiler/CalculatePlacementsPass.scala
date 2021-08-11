// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

import scala.collection.mutable.{ArrayBuffer}

class CalculatePlacementsPass(topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val tree = new FloorplanTree(state, topMod)

    val top = tree.topNode
    assert(top.record.element.isInstanceOf[SizedHierarchicalTop], "PlacedHierarchicalTop not yet supported pre-CalculatePlacementsPass. Use SizedHierarchicalTop instead.")

    val elementBuf = new ArrayBuffer[Element]()

    // Custom traversal TODO should this go in the FloorplanTree code somehow?
    def traverse(n: FloorplanTreeNode, x: BigDecimal, y: BigDecimal, placeTopOpt: Option[FloorplanTreeNode]) {
      // TODO this needs to be cleaned up elsewhere
      val legalX = x.setScale(3, BigDecimal.RoundingMode.HALF_UP)
      val legalY = y.setScale(3, BigDecimal.RoundingMode.HALF_UP)
      n.record.element match {
        case e: SizedHierarchicalTop =>
          assert(placeTopOpt.isEmpty)
          // Note: we intentionally wait to add this until the end so we know all of the elements
          val children = n.children
          assert(children.length == 1) // should only be a topGroup here
          children.foreach { cNode => traverse(cNode, x, y, Some(n)) }
          n.replace(n.record.copy(element = PlacedHierarchicalTop(
            e.name,
            elementBuf.toSeq.map(_.name),
            legalX,
            legalY,
            e.width,
            e.height,
            e.margins,
            e.hardBoundary
          )))
        case e: SizedGrid =>
          assert(placeTopOpt.isDefined)
          // traverse down
          val nodes: Seq[FloorplanTreeNode] = e.elements.map(name => tree.getNode(name))
          val children: Seq[SizedRectLike] = nodes.map(_.record.element.asInstanceOf[SizedRectLike])
          val widths: Seq[BigDecimal] = children.take(e.xDim).map(_.width).scanLeft(BigDecimal(0))(_+_).take(e.xDim)
          val heights: Seq[BigDecimal] = children.grouped(e.xDim).map(_(0).height).toSeq.scanLeft(BigDecimal(0))(_+_).take(e.yDim)

          nodes.zipWithIndex.foreach { case (node, idx) =>
            val (iX, iY) = e.fromIdx(idx)
            traverse(node, legalX + widths(iX), legalY + heights(iY), placeTopOpt)
          }

          // delete it
          n.delete()
        case e: SizedLogicRect =>
          assert(placeTopOpt.isDefined)
          n.replace(n.record.copy(element = PlacedLogicRect(
            e.name,
            e.parent,
            legalX,
            legalY,
            e.width,
            e.height,
            e.hardBoundary
          )))
          n.reparent(placeTopOpt.get)
          elementBuf.append(n.record.element)
        case e: SizedSpacerRect =>
          assert(placeTopOpt.isDefined)
          // delete it
          n.delete()
        case e: SizedMacro =>
          assert(placeTopOpt.isDefined)
          n.replace(n.record.copy(element = PlacedMacro(
            e.name,
            e.parent,
            e.orientation,
            legalX,
            legalY,
            e.width,
            e.height
          )))
          n.reparent(placeTopOpt.get)
          elementBuf.append(n.record.element)
        case e => ??? // Shouldn't get here
      }
    }

    traverse(top, BigDecimal(0), BigDecimal(0), None)

    tree.toState
  }
}
