// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class ResolveConstraintsPass(topMod: String) extends Pass {
  def resolveConstraints(elt: ConstrainedRectLike): (BigDecimal, BigDecimal) = {
    // TODO this is a temporary strategy
    elt.toConstraints.resolveMinDimensions()
  }

  def execute(state: FloorplanState): FloorplanState = {
    val tree = new FloorplanTree(state, topMod)

    // TODO this isn't a correct implementation; it just resolves the constraints seen
    // by any individual element rather than looking at the entirety of the floorplan

    // Bottom-up pass
    tree.traverseMapPost { node =>
      node.record.element match {
        case e: SizedRectLike => None // Already sized
        case e: ConstrainedSpacerRect =>
          val (width, height) = resolveConstraints(e)
          Some(node.record.copy(element = SizedSpacerRect(
            e.name,
            e.parent,
            width,
            height
          )))
        case e: ConstrainedLogicRect =>
          val (width, height) = resolveConstraints(e)
          Some(node.record.copy(element = SizedLogicRect(
            e.name,
            e.parent,
            width,
            height,
            e.hardBoundary
          )))
        case e: ConstrainedHierarchicalTop =>
          val (width, height) = resolveConstraints(e)
          Some(node.record.copy(element = PlacedHierarchicalTop(
            e.name,
            Seq(e.topGroup),
            width,
            height,
            e.margins,
            e.hardBoundary
          )))
        case e: ConstrainedWeightedGrid =>
          // TODO make not repetitive
          // Preserve ordering of children and convert to 2d Seq
          // FIXME this is a hack that assumes all of the elements are equally constrained in a row (height) or column (width)
          // This is true for the current implementation of the constraints propagation but not generally true
          val childDims: Seq[(BigDecimal, BigDecimal)] = e.elements.map(x => tree.getRecord(x).element.toConstraints.resolveMinDimensions())
          val widths = childDims.take(e.xDim).map(_._1)
          val heights = childDims.grouped(e.xDim).map(_(0)._2).toSeq

          Some(node.record.copy(element = SizedGrid(
            e.name,
            e.parent,
            e.xDim,
            e.yDim,
            e.elements,
            widths,
            heights
          )))
        case e: ConstrainedElasticGrid =>
          // Preserve ordering of children and convert to 2d Seq
          // FIXME this is a hack that assumes all of the elements are equally constrained in a row (height) or column (width)
          // This is true for the current implementation of the constraints propagation but not generally true
          val childDims: Seq[(BigDecimal, BigDecimal)] = e.elements.map(x => tree.getRecord(x).element.toConstraints.resolveMinDimensions())
          val widths = childDims.take(e.xDim).map(_._1)
          val heights = childDims.grouped(e.xDim).map(_(0)._2).toSeq

          Some(node.record.copy(element = SizedGrid(
            e.name,
            e.parent,
            e.xDim,
            e.yDim,
            e.elements,
            widths,
            heights
          )))
        case e => throw new Exception("Illegal element type")
      }
    }

    tree.toState
  }
}
