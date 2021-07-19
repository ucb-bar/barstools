// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class ConstraintPropagationPass(val topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val tree = new FloorplanTree(state, topMod)

    // TODO This probably should use a SAT solver or something fancier

    // Propagate macro sizes to MemMacroArrays
    val nodes = tree.allNodes.filter({ case (k,v) =>
      v.record.element.isInstanceOf[MemMacroArray]
    }).values
    nodes.foreach { node =>
      val element = node.record.element.asInstanceOf[MemMacroArray]
      node.replace(node.record.copy(
        element = element.applyConstraints(Constraints(
          width = Unconstrained(),
          height = Unconstrained(),
          area = GreaterThanOrEqualTo(element.elements.map(e =>
            tree.getRecord(e.get).element match {
              case e2: AbstractMacro => throw new Exception("Macros need to be sized")
              case e2: SizedMacro => e2.width*e2.height
              case e2: PlacedMacro => e2.width*e2.height
              case _ => ???
            }
          ).reduce(_+_)),
          aspectRatio = Unconstrained()
        ))
      ))
    }


    // Top-down pass
    tree.traverseMapPre { node  =>
      val constraints: Constraints = node.parent.map(_.record.element match {
        case e: ConstrainedHierarchicalTop =>
          Constraints(e.width, e.height, e.area, e.aspectRatio)
        case e: PlacedHierarchicalTop =>
          Constraints.sized(e.width, e.height)
        case e: ConstrainedWeightedGrid =>
          val width = e.width(e.flatIndexOf(node.record.element.name))
          val height = e.height(e.flatIndexOf(node.record.element.name))
          val area = e.area(e.flatIndexOf(node.record.element.name))
          val aspectRatio = e.aspectRatio(e.flatIndexOf(node.record.element.name))
          Constraints(width, height, area, aspectRatio)
        case e: ConstrainedElasticGrid =>
          val width = e.width(e.flatIndexOf(node.record.element.name))
          val height = e.height(e.flatIndexOf(node.record.element.name))
          val area = e.area(e.flatIndexOf(node.record.element.name))
          val aspectRatio = e.aspectRatio(e.flatIndexOf(node.record.element.name))
          Constraints(width, height, area, aspectRatio)
        case e: SizedGrid =>
          val (x, y) = e.indexOf(node.record.element.name).get
          Constraints.sized(e.widths(x), e.heights(y))
        case e: MemMacroArray =>
          Constraints() // These *should* be hard macros at this point, so no need to constrain them. Maybe aspect ratio?
        case _ => ??? // Many types can never be parents and shouldn't get here
      }).getOrElse(Constraints())
      // Only modify child
      (None, Some(node.record.copy(element = node.record.element.applyConstraints(constraints))))
    }

    // Bottom-up pass
    tree.traverseMapPost { node =>
      // Get idx in parent
      val idx = node.parent.map(_.record.element.flatIndexOf(node.record.element.name)).getOrElse(-1)

      val constraints: Constraints = node.record.element match {
        case e: ConstrainedSpacerRect =>
          Constraints(e.width, e.height, e.area, e.aspectRatio)
        case e: SizedSpacerRect =>
          Constraints.sized(e.width, e.height)
        case e: ConstrainedLogicRect =>
          Constraints(e.width, e.height, e.area, e.aspectRatio)
        case e: SizedLogicRect =>
          Constraints.sized(e.width, e.height)
        case e: PlacedLogicRect =>
          Constraints.sized(e.width, e.height)
        case e: ConstrainedHierarchicalTop =>
          Constraints(e.width, e.height, e.area, e.aspectRatio)
        case e: PlacedHierarchicalTop =>
          Constraints.sized(e.width, e.height)
        case e: ConstrainedWeightedGrid =>
          val width = e.width.reduce(_+_)
          val height = e.height.reduce(_+_)
          val area = e.area.reduce(_+_)
          Constraints(width, height, area, Unconstrained())
        case e: ConstrainedElasticGrid =>
          val width = e.width.reduce(_+_)
          val height = e.height.reduce(_+_)
          val area = e.area.reduce(_+_)
          Constraints(width, height, area, Unconstrained())
        case e: SizedGrid =>
          Constraints.sized(e.widths.sum, e.heights.sum)
        case e: MemMacroArray =>
          Constraints(e.width, e.height, e.area, e.aspectRatio)
        case e: SizedMacro =>
          Constraints.sized(e.width, e.height)
        case e: PlacedMacro =>
          Constraints.sized(e.width, e.height)
        case _ => ???
      }

      val newElementOpt = node.parent.map(_.record.element match {
        case e: Grid => e.applyConstraintsTo(constraints, e.flatIndexOf(node.record.element.name))
        case e: ConstrainedHierarchicalTop => e.applyConstraints(constraints)
        case e: PlacedHierarchicalTop => e.applyConstraints(constraints)
        case e => e
      })
      // Only modify parent
      (newElementOpt.map(e => node.record.copy(element = e)), None)
    }

    // TODO propagate constraints to siblings

    tree.toState
  }
}
