// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

import scala.math.{BigDecimal, sqrt}

case class AreaConstraintViolationException(msg: String) extends Exception(msg)
case class AspectRatioIncompatibleException(msg: String) extends Exception(msg)

class ReplaceMemMacroArrayPass(topMod: String) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val tree = new FloorplanTree(state, topMod)
    // TODO this needs some additional options, etc.
    val utilization = 0.8 // amount of area to devote to SRAMs

    // For the proof-of-concept, we'll just replace them with an Array that meets whatever box constraints
    val nodes = tree.allNodes.values.filter(_.record.element.isInstanceOf[MemMacroArray])

    nodes.foreach { n =>
      val e = n.record.element.asInstanceOf[MemMacroArray]
      // TODO this assumes that the MemMacroArray contains all the same type of SRAM. This is usually true, but we do nothing to enforce this.
      // Check if all the SRAMs are the same type
      val sram_width = tree.getRecord(e.elements(0)).element.asInstanceOf[SizedMacro].width
      val sram_height = tree.getRecord(e.elements(0)).element.asInstanceOf[SizedMacro].height
      for (i <- e.elements) {
          val next_sram = tree.getRecord(i).element.asInstanceOf[SizedMacro]
          assert(next_sram.width == sram_width && next_sram.height == sram_height, "The SRAMs in " + e.name + " module have multiple types. This floorplan complier only supports same type of SRAMs")
      }


      val constrainedArea = e.height.getConstraint * e.width.getConstraint
      if (constrainedArea < e.area.getConstraint) {
          throw AreaConstraintViolationException("Area Constraint Violation is violated. The constraintArea(" + constrainedArea + ") is smaller than required area for all SRAMs(" + e.area.getConstraint + "). Please give more budget to this module.")
      }

      // Try to size based on the SRAM
      val mem0 = tree.getRecord(e.elements(0)).element.asInstanceOf[SizedMacro]
      val numMems = e.elements.length

      val defaultWidth = (mem0.width * numMems) / utilization
      val defaultHeight = mem0.height
      val defaultArea = defaultWidth * defaultHeight
    
      val (width, height) = e.toConstraints.aspectRatio match {
        case c: Unconstrained =>
          (defaultWidth, defaultHeight)
        case c: Impossible => throw new Exception("Illegal impossible constraint")
        case c: Constrained =>
          val h = c.eq.map(q => BigDecimal(sqrt((defaultArea * q).doubleValue))).getOrElse(defaultHeight)
          (defaultArea/h, h)
      }

      val stackHeight = (height / mem0.height).setScale(0, BigDecimal.RoundingMode.DOWN).toInt
      val columns = (numMems + stackHeight - 1) / Seq(stackHeight, 1).max
      val paddingColumns = (columns + 1) / 2
      val topPadding = height - (stackHeight*mem0.height)
      val xPadding = (width - (columns*mem0.width)) / paddingColumns
      val xDim = columns + paddingColumns
      val yDim = stackHeight + 1
      printf("==================================ReplaceMemMacroArrayPass=========================" + "\n")
      printf("name: " + e.name + "\n")
      printf("topPadding: " + topPadding + "\n")
      printf("xPadding: " + xPadding + "\n")
      printf("height: " + height + "\n")
      printf("width: " + width + "\n")
      printf("stackHeight: " + stackHeight + "\n")
      printf("columns: " + columns + "\n")
      printf("xDim: " + xDim + ", yDim: " + yDim + "\n")
      printf("default area: " + defaultArea + "\n")
      printf("mem0 w and h: " + mem0.width + ", " + mem0.height + "\n")
      printf("numMems: " + numMems + "\n")
      printf("default Width: " + defaultWidth + "(" + mem0.width * numMems + " )" + ", default height: " + defaultHeight + "\n")
        
      if (topPadding < 0 || xPadding <= 0) {
          throw AspectRatioIncompatibleException("The aspect ratio can not be satisfied. Please try another aspect ratio for module: " + e.name)
      }

      def addSpacer(w: BigDecimal, h: BigDecimal): String = {
        val newElement = SizedSpacerRect(
          name = tree.getUniqueName("spacer"),
          parent = e.name,
          width = w,
          height = h
        )
        n.addChildRecord(FloorplanRecord(n.record.scope, None, None, newElement))
        newElement.name
      }

      val elements = Seq.tabulate(xDim*yDim) { i =>
        val col = (i % xDim)
        val row = (i / xDim)
        val group = 2*(col / 3) + (row*columns)
        val spacerHeight = if (row == yDim-1) topPadding else mem0.height
        if (col % 3 == 0) {
          if (group >= numMems) {
            addSpacer(xPadding, spacerHeight)
          } else {
            e.elements(group)
          }
        } else if (col % 3 == 1) {
          addSpacer(xPadding, spacerHeight)
        } else {
          if ((group + 1) >= numMems) {
            addSpacer(xPadding, spacerHeight)
          } else {
            val eOut = e.elements(group + 1)
            // Mirror it for proof-of-concept
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
      n.replace(n.record.copy(element = newElement))
    }
    tree.toState
  }
}
