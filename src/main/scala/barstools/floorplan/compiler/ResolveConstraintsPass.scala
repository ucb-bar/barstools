// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._
import scala.math.{BigDecimal, sqrt}

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
          Some(node.record.copy(element = SizedHierarchicalTop(
            e.name,
            e.topGroup,
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
          val heights = childDims.take(e.xDim).map(_._2)

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
          // This is a hack since ConstrainedElasticGrid is always 1D
          val widths = childDims.take(e.yDim.max(e.xDim)).map(_._1)
          val heights = childDims.take(e.yDim.max(e.xDim)).map(_._2)
          println("+++++++++++: " , widths , "++++++++++++ \n")

          // Optimization: Try to adjust aspect ratio if input constraints are violated
          // PL represent node that only contains MemMacroArray
          var is_PL = true
          for (ele <- e.elements) {
              tree.getNode(ele).record.element match {
                  case z: SizedGrid => 
                    tree.getNode(z.elements(0)).record.element match {
                        case x: SizedMacro =>
                        case x => is_PL = false
                    }
                  case z: SizedSpacerRect =>
                  case z  => is_PL = false
              }
          }

          // Indicate which modules are PL
          println("is_PL: ", is_PL, "   name of the module:", e.name)
          
          // Detect violations early 
          val hConstraint = e.height.getConstraint
          val wConstraint = e.width.getConstraint

          val val_width = widths.reduce(_+_)
          val val_height = heights.reduce(_+_)
          println("Height Constraint: " + hConstraint + ", Width Constraint: " + wConstraint + "\n")
          println("Actual Height: " + val_height + ", Actual Width: " + val_width + "\n")
          if (hConstraint < val_height & wConstraint < val_width) {
              throw new Exception("Module: " + e.name + " This module needs more space, try to give more height and width to the constraint.")
          } else if (hConstraint < val_height) {
              if (is_PL == false) {
                throw new Exception("Module: " + e.name + " This module need more htight to be placed, try to give more height to the constraint.")
              } else {
                 var height_space: BigDecimal = 0
                 for (ele <- e.elements) {
                     tree.getNode(ele).record.element match {
                        case z: SizedSpacerRect => height_space = height_space + z.height
                        case z =>
                     }
                 }
                 val new_height_cons = hConstraint - height_space
                 Some(node.record.copy(element = tryasp(new_height_cons, wConstraint, node, 1, 3, tree)))
              }


          } else if (wConstraint < val_width) {
              if (is_PL == false) {
                throw new Exception("Module: " + e.name + " This module needs more width, try to give more height to the constraint.")
              } else {
                  printf("=========================calling the function on================" + e.elements + "\n")
                  var width_space: BigDecimal = 0
                  node.children.foreach{cNode =>
                    cNode.record.element match {
                        case z: SizedSpacerRect =>  width_space = width_space + z.width
                        case z => 
                    }
                  }
                  for (ele <- e.elements) {
                      tree.getNode(ele).record.element match {
                          case z: ConstrainedSpacerRect => width_space = width_space + z.width.getConstraint; 
                          case z =>
                      }
                  }
                  val new_width_cons = wConstraint - width_space
                  Some(node.record.copy(element = tryasp(hConstraint, new_width_cons, node, 0, 3, tree))) 

              }

          } else {
            Some(node.record.copy(element = SizedGrid(
                e.name,
                e.parent,
                e.xDim,
                e.yDim,
                e.elements,
                widths,
                heights
            )))
          }
        case e => throw new Exception("Illegal element type")
      }
    }

    tree.toState
  }
  def tryasp(height_constraint: BigDecimal, width_constraint: BigDecimal, elastic_array: FloorplanTreeNode, h_or_w: BigDecimal, ttl: BigDecimal, tree: FloorplanTree): SizedGrid = {
    printf("==================Enter Tryasp===================\n")
    if (ttl == 0) {
        throw new Exception("Module: " + elastic_array.record.element.name + " Failed to change aspect ratio to resolve constraints. Please give more space to this module.")
    }
      
    var memMacroArrays = elastic_array.children.filter(_.record.element.isInstanceOf[SizedGrid])

    // helper function
    def addSpacer(w: BigDecimal, h: BigDecimal, n: FloorplanTreeNode, e: SizedGrid): String = {
      val newElement = SizedSpacerRect(
        name = tree.getUniqueName("spacer"),
        parent = e.name,
        width = w,
        height = h
     )
      n.addChildRecord(FloorplanRecord(n.record.scope, None, None, newElement))
      newElement.name
    }


    // track new height and width
    var new_height: BigDecimal = 0
    var new_width: BigDecimal = 0

    for (arr <- memMacroArrays) {
          val node = arr
          val children = node.children
          val e = node.record.element.asInstanceOf[SizedGrid]


          var spacer_names = List.empty[String]
          children.foreach{ child =>
            child.record.element match {
                case SizedSpacerRect(_,_,_,_) => spacer_names = spacer_names :+ child.record.element.name
                case _ =>
            }
          }
          for(name <- spacer_names) {
              val nd = tree.getNode(name)
              nd.delete()
              e.elements.filterNot(_ == name)
          }

          // for holding elements without spacers
          var nonspacerElementHolder = Seq.empty[String]
          for(ele <- e.elements) {
              if (!spacer_names.contains(ele)) {
                  nonspacerElementHolder = nonspacerElementHolder :+ ele
              }
          }


          // find out orignal aspect ratio
          val old_aspect_ratio = e.height / e.width
          var new_aspect_ratio: BigDecimal = 1
          if (h_or_w == 0) {
            new_aspect_ratio = old_aspect_ratio * 1.1
          } else {
            new_aspect_ratio = old_aspect_ratio * 0.9
          }
          // Used for debugging aspect ratio
          printf("===============new Width, Height, and Asp Ratio================" + e.width + "   " + e.height + "     " + new_aspect_ratio +  "\n") 
          // construct new MemMacroArray
          val mem0 = tree.getRecord(e.elements(0)).element.asInstanceOf[SizedMacro]
          val numMems = node.children.length
          // utilization is hard-coded maybe we can change this as well?
          val utilization = 0.8
          val defaultWidth = (mem0.width * numMems) / utilization
          val defaultHeight = mem0.height
          val defaultArea = defaultWidth * defaultHeight
          val height = BigDecimal(sqrt((defaultArea * new_aspect_ratio).doubleValue))
          val width = defaultArea / height
          val stackHeight = (height / mem0.height).setScale(0, BigDecimal.RoundingMode.DOWN).toInt
          val columns = (numMems + stackHeight - 1) / Seq(stackHeight, 1).max
          val paddingColumns = (columns + 1) / 2
          val topPadding = height - (stackHeight*mem0.height)
          val xPadding = (width - (columns*mem0.width)) / paddingColumns
          val xDim = columns + paddingColumns
          val yDim = stackHeight + 1


          // elastic_array is a TreeNode
          if (topPadding < 0 || xPadding <= 0) {
              throw new Exception("Module: " + elastic_array.record.element.name + "  takes too much space, please adjust constraint accordingly.")
          }
          val elements = Seq.tabulate(xDim*yDim) { i =>
            val col = (i % xDim)
            val row = (i / xDim)
            val group = 2*(col / 3) + (row*columns)
            val spacerHeight = if (row == yDim - 1) topPadding else mem0.height
            if (col % 3 == 0) {
                if (group >= numMems) {
                    addSpacer(xPadding, spacerHeight, node, e)
                } else {
                    nonspacerElementHolder(group)
                }
            } else if (col % 3 == 1) {
                addSpacer(xPadding, spacerHeight, node, e)
            } else {
                if ((group + 1) >= numMems) {
                    addSpacer(xPadding, spacerHeight, node, e)
                } else {
                    val eOut = nonspacerElementHolder(group + 1)
                    val myNode = tree.getNode(eOut)
                    myNode.replace(myNode.record.copy(element = myNode.record.element.asInstanceOf[SizedMacro].copy(
                        orientation = Orientation.my
                    )))
                    eOut
                }
            }
          }
          val widths = Seq.tabulate(xDim) { i =>
            if (i % 3 == 0) {
                mem0.width
            } else if (i % 3 == 1) {
                xPadding
            } else {
                mem0.width
            }
          }
          val heights = Seq.fill(stackHeight)(mem0.height) ++ Seq(topPadding)
          val newElement = SizedGrid(
            name = e.name,
            parent = e.parent,
            xDim = xDim,
            yDim = yDim,
            elements = elements,
            widths = widths,
              heights = heights
          )
          node.replace(node.record.copy(element = newElement))
          new_height = new_height + heights.reduce(_+_)
          new_width = new_width + widths.reduce(_+_)
    }
    // Resolve the resulf of change aspect ratio
    if (new_height < height_constraint && new_width < width_constraint) {
      val childDims: Seq[(BigDecimal, BigDecimal)] = elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].elements.map(x => tree.getRecord(x).element.toConstraints.resolveMinDimensions())
      val widths = childDims.take(elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].xDim).map(_._1)
      val heights = childDims.take(elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].xDim).map(_._2)
      val res = SizedGrid(
          name = elastic_array.record.element.name,
          parent = elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].parent,
          xDim = elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].xDim,
          yDim = elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].yDim,
          elements = elastic_array.record.element.asInstanceOf[ConstrainedElasticGrid].elements,
          widths = widths,
          heights = heights
          )
      res
          
    } else {
        tryasp(height_constraint, width_constraint, elastic_array, h_or_w, ttl - 1, tree)
    }
    
  }
}
