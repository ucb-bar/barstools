// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._
import scala.collection.Map

class SidebandAnnotationPass(val sbMap: Map[String, SidebandAnnotation]) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val newRecords = state.records.map { record =>
      val newElement = record.element match {
        case e: ConstrainedLogicRect =>
          sbMap.get(e.ofModule).map(sb =>
            e.copy(
              width = e.width.and(sb.widthConstraint),
              height = e.height.and(sb.heightConstraint),
              area = e.area.and(sb.areaConstraint)
            )
          ).getOrElse(e)
        case e: SizedLogicRect =>
          sbMap.get(e.ofModule).map(sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          ).getOrElse(e)
        case e: PlacedLogicRect =>
          sbMap.get(e.ofModule).map(sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          ).getOrElse(e)
        case e: ConstrainedHierarchicalTop =>
          sbMap.get(e.ofModule).map(sb =>
            e.copy(
              width = e.width.and(sb.widthConstraint),
              height = e.height.and(sb.heightConstraint),
              area = e.area.and(sb.areaConstraint)
            )
          ).getOrElse(e)
        case e: PlacedHierarchicalTop =>
          sbMap.get(e.ofModule).map({sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          }).getOrElse(e)
        case e: AbstractMacro =>
          sbMap.get(e.ofModule).map({sb =>
            assert(sb.width.isDefined && sb.height.isDefined, "Macro sideband annotations must include width and height")
            SizedMacro(
              name = e.name,
              ofModule = e.ofModule,
              width = sb.width.get,
              height = sb.height.get
            )
          }).getOrElse(e)
        case e: SizedMacro =>
          sbMap.get(e.ofModule).map({sb =>
            assert(sb.width.isDefined && sb.height.isDefined, "Macro sideband annotations must include width and height")
            e.copy(
              width = sb.width.get,
              height = sb.height.get
            )
          }).getOrElse(e)
        case e: PlacedMacro =>
          sbMap.get(e.ofModule).map({sb =>
            assert(sb.width.isDefined && sb.height.isDefined, "Macro sideband annotations must include width and height")
            e.copy(
              width = sb.width.get,
              height = sb.height.get
            )
          }).getOrElse(e)
        case e => e
      }
      record.copy(element = newElement)
    }
    state.copy(records = newRecords)
  }
}

