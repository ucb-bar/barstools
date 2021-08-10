// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class ResolveConstraintsPass(topMod: String) extends Pass {
  def resolveConstraints(elt: ConstrainedRectLike): (BigDecimal, BigDecimal) = {
    ???
  }

  def execute(state: FloorplanState): FloorplanState = {
    val tree = new FloorplanTree(state, topMod)

    // TODO this isn't a correct implementation; it just resolves the constraints seen
    // by any individual element rather than looking at the entirety of the floorplan

    /*
    // Bottom-up pass
    tree.traverseMapPost { node =>
      node.record.element match {
        case e: SizedRectLike => e // Already sized
        case e: ConstrainedSpacerRect =>
          val (width, height) = resolveConstraints(e)
          SizedSpacerRect(
            e.name,
            e.parent,
            width,
            height
          )
        case e: ConstrainedLogicRect =>
          val (width, height) = resolveConstraints(e)
          SizedLogicRect(
            e.name,
            e.parent,
            width,
            height,
            hardBoundary
          )
        case e: ConstrainedHierarchicalTop =>
          val (width, height) = resolveConstraints(e)
          PlacedHierarchicalTop(
            e.name,
            e.elements = Seq(e.topGroup),
            width,
            height,
            e.margins,
            e.hardBoundary
          )
        case e: ConstrainedWeightedGrid =>
          // Preserve ordering of children and convert to 2d Seq
          val children: Seq[Seq[Constraints]] = e.elements.map(_.map(x => tree.getRecord(x).element.toConstraints).getOrElse(Constraints())).grouped(e.xDim).toSeq
          //SizedGrid(TODO)
          ???
        case e: ConstrainedElasticGrid =>
          //SizedGrid(TODO)
          ???
        case e => throw new Exception("Illegal element type")
      }
    }
    */

    tree.toState
  }
}
