// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class ConstraintPropagationPass(val topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val tree = new FloorplanTree(state, topMod)

    // Top-down pass
    tree.traverseMapPre { node  =>
      val constraints: Constraints = node.parent.map(_.record.element match {
        case e: ConstrainedHierarchicalTop =>
          e.toConstraints
        case e: PlacedHierarchicalTop =>
          Constraints() // These should be sized already
        case e: ConstrainedWeightedGrid =>
          val (x, y) = e.indexOf(node.record.element.name).get
          val xWeight = e.getXWeight(x)
          val yWeight = e.getYWeight(y)
          e.toConstraints.weightXY(xWeight, yWeight)
        case e: ConstrainedElasticGrid =>
          val c = e.toConstraints
          val widthConstraint = c.width match {
            case x: Unconstrained => x
            case x: Impossible => x
            case x: Constrained => x.copy(eq = None, leq = x.eq.orElse(x.leq), geq = None, mof = None)
          }
          val heightConstraint = c.height match {
            case x: Unconstrained => x
            case x: Impossible => x
            case x: Constrained => x.copy(eq = None, leq = x.eq.orElse(x.leq), geq = None, mof = None)
          }
          val areaConstraint = c.area match {
            case x: Unconstrained => x
            case x: Impossible => x
            case x: Constrained => x.copy(eq = None, leq = x.eq.orElse(x.leq), geq = None, mof = None)
          }
          Constraints(width = widthConstraint, height = heightConstraint, area = areaConstraint, aspectRatio = Unconstrained())
        case e: SizedGrid =>
          val (x, y) = e.indexOf(node.record.element.name).get
          Constraints.sized(e.widths(x), e.heights(y))
        case e: MemMacroArray =>
          Constraints() // These *should* be hard macros at this point, so no need to constrain them. Maybe aspect ratio?
        case _ => ??? // Many types can never be parents and shouldn't get here
      }).getOrElse(Constraints())

      // Only modify child
      val newElement = node.record.element match {
        case e: Constrainable =>
          e.applyConstraints(constraints)
        case e => ???
      }
      Some(node.record.copy(element = newElement))
    }


    // Bottom-up pass
    tree.traverseMapPost { node =>
      node.record.element match {
        case e: ConstrainedSpacerRect =>
          None
        case e: SizedSpacerRect =>
          None
        case e: ConstrainedLogicRect =>
          None
        case e: SizedLogicRect =>
          None
        case e: PlacedLogicRect =>
          None
        case e: ConstrainedHierarchicalTop =>
          val newElement = e.applyConstraints(node.children(0).record.element.toConstraints)
          Some(node.record.copy(element = newElement))
        case e: PlacedHierarchicalTop =>
          throw new Exception("Cannot propagate constraints to a PlacedHierarchicalTop")
        case e: ConstrainedWeightedGrid =>
          // TODO make this less repetitive with the below
          // Preserve ordering of children and convert to 2d Seq
          val children: Seq[Seq[Constraints]] = e.elements.map(_.map(x => tree.getRecord(x).element.toConstraints).getOrElse(Constraints())).grouped(e.xDim).toSeq
          val widthConstraint = children.map(_.map(_.width).reduce(_ + _)).reduce(_ and _)
          val heightConstraint = children.transpose.map(_.map(_.height).reduce(_ + _)).reduce(_ and _)
          val areaConstraint = children.flatten.map(_.area).reduce(_ + _)
          val newElement = e.applyConstraints(Constraints(
            widthConstraint,
            heightConstraint,
            areaConstraint,
            Unconstrained()
          ))
          // TODO handle the weights
          Some(node.record.copy(element = newElement))
        case e: ConstrainedElasticGrid =>
          // Preserve ordering of children and convert to 2d Seq
          val children: Seq[Seq[Constraints]] = e.elements.map(_.map(x => tree.getRecord(x).element.toConstraints).getOrElse(Constraints())).grouped(e.xDim).toSeq
          val widthConstraint = children.map(_.map(_.width).reduce(_ + _)).reduce(_ and _)
          val heightConstraint = children.transpose.map(_.map(_.height).reduce(_ + _)).reduce(_ and _)
          val areaConstraint = children.flatten.map(_.area).reduce(_ + _)
          val newElement = e.applyConstraints(Constraints(
            widthConstraint,
            heightConstraint,
            areaConstraint,
            Unconstrained()
          ))
          Some(node.record.copy(element = newElement))
        case e: SizedGrid =>
          ??? // TODO probably should sanity check here
        case e: MemMacroArray =>
          val area = node.children.map(_.record.element match {
            case e2: HasFixedDimensions => e2.area
            case e2 => throw new Exception("All macro dimensions must be resolved at this point")
          }).sum
          val newElement = e.applyConstraints(Constraints(
            Unconstrained(),
            Unconstrained(),
            GreaterThanOrEqualTo(area),
            Unconstrained()
          ))
          Some(node.record.copy(element = newElement))
        case e: SizedMacro =>
          None
        case e: PlacedMacro =>
          None
        case _ => ???
      }
    }

    // Sibling propagation to Grids
    tree.allNodes.values.foreach { node =>
      node.record.element match {
        case e: SizedGrid =>
          // Do nothing
        case e: Grid =>
          // This look-up preserves ordering of children
          val children = e.elements.map(_.map(x => tree.getNode(tree.getRecord(x).element.name)))
          val wConstraints = Seq.tabulate(e.xDim)(iX => Seq.tabulate(e.yDim) { iY =>
            children(e.toIdx(iX, iY)).map(_.record.element.toConstraints.width).getOrElse(Unconstrained())
          } reduce (_ and _))
          val hConstraints = Seq.tabulate(e.yDim)(iY => Seq.tabulate(e.xDim) { iX =>
            children(e.toIdx(iX, iY)).map(_.record.element.toConstraints.height).getOrElse(Unconstrained())
          } reduce (_ and _))
          val newElements = children.zipWithIndex.map { case (child, idx) =>
            val (iX, iY) = e.fromIdx(idx)
            if (child.isDefined) {
              // We can always assume this is Constrainable- but really we should fix this in the type system
              child.get.replace(child.get.record.copy(
                element = child.get.record.element.asInstanceOf[Constrainable].
                  applyConstraints(Constraints(width = wConstraints(iX), height = hConstraints(iY), Unconstrained(), Unconstrained()))
              ))
              Some(child.get.record.element.name)
            } else {
              val newElement = ConstrainedSpacerRect(tree.getUniqueName("spacer"), e.name).
                applyConstraints(Constraints(width = wConstraints(iX), height = hConstraints(iY), Unconstrained(), Unconstrained()))
              node.addChildRecord(FloorplanRecord(node.record.scope, None, None, newElement))
              Some(newElement.name)
            }
          }
          // Needed to be able to use copy
          e match {
            case e: ConstrainedWeightedGrid =>
              node.replace(node.record.copy(element = e.copy(elements = newElements)))
            case e: ConstrainedElasticGrid =>
              node.replace(node.record.copy(element = e.copy(elements = newElements)))
            case e =>
              ???
          }
        case e =>
          // Do nothing
      }
    }

    tree.toState
  }
}
