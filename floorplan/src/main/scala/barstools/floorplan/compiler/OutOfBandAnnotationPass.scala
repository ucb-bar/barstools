// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._
import scala.collection.Map

class OutOfBandAnnotationPass(val sbMap: Map[String, OutOfBandAnnotation]) extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val newRecords = state.records.map { record =>
      val ofModule = record.ofModule.getOrElse("")
      val newElement = record.element match {
        case e: ConstrainedLogicRect =>
          sbMap.get(ofModule).map(sb =>
            e.copy(
              width = e.width.and(sb.widthConstraint),
              height = e.height.and(sb.heightConstraint),
              area = e.area.and(sb.areaConstraint)
            )
          ).getOrElse(e)
        case e: SizedLogicRect =>
          sbMap.get(ofModule).map(sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          ).getOrElse(e)
        case e: PlacedLogicRect =>
          sbMap.get(ofModule).map(sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          ).getOrElse(e)
        case e: ConstrainedHierarchicalTop =>
          sbMap.get(ofModule).map(sb =>
            e.copy(
              width = e.width.and(sb.widthConstraint),
              height = e.height.and(sb.heightConstraint),
              area = e.area.and(sb.areaConstraint)
            )
          ).getOrElse(e)
        case e: SizedHierarchicalTop =>
          sbMap.get(ofModule).map({sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          }).getOrElse(e)
        case e: PlacedHierarchicalTop =>
          sbMap.get(ofModule).map({sb =>
            e.copy(
              width = sb.width.getOrElse(e.width),
              height = sb.height.getOrElse(e.height)
            )
          }).getOrElse(e)
        case e: AbstractMacro =>
          sbMap.get(ofModule).map({sb =>
            assert(sb.width.isDefined && sb.height.isDefined, "Macro out-of-band annotations must include width and height")
            SizedMacro(
              name = e.name,
              parent = e.parent,
              orientation = Orientation.r0,
              width = sb.width.get,
              height = sb.height.get
            )
          }).getOrElse(e)
        case e: SizedMacro =>
          sbMap.get(ofModule).map({sb =>
            assert(sb.width.isDefined && sb.height.isDefined, "Macro out-of-band annotations must include width and height")
            e.copy(
              width = sb.width.get,
              height = sb.height.get
            )
          }).getOrElse(e)
        case e: PlacedMacro =>
          sbMap.get(ofModule).map({sb =>
            assert(sb.width.isDefined && sb.height.isDefined, "Macro out-of-band annotations must include width and height")
            e.copy(
              width = sb.width.get,
              height = sb.height.get
            )
          }).getOrElse(e)
        case e => e
      }
      record.copy(element = newElement)
    }
    newRecords.map(_.element).collectFirst {
      case e: AbstractMacro =>
        throw new Exception("Unannotated macros still exist after OutOfBandAnnotationPass")
    }
    state.copy(records = newRecords)
  }
}

