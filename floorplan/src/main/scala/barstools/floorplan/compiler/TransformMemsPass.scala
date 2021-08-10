// See LICENSE for license details
package barstools.floorplan.compiler

import firrtl.annotations.TargetToken.{Instance, OfModule}

import barstools.floorplan._

import scala.collection.{Map, Set}
import scala.collection.mutable.{HashMap, HashSet}

class TransformMemsPass(instMap: Map[OfModule, Seq[(Instance, OfModule)]]) extends Pass {

  val nameSet = new HashSet[String]()
  val renameMap = new HashMap[String, Set[String]]()

  def getUniqueName(suggestion: String): String = {
    // This is ugly and probably slow
    var i = 0
    var tempName = suggestion + s"_${i}"
    while (nameSet.contains(tempName)) {
      tempName = suggestion + s"_${i}"
      i += 1
    }
    nameSet.add(tempName)
    tempName
  }

  def execute(state: FloorplanState): FloorplanState = {
    nameSet ++= state.records.map(_.element.name).toSet
    // Need to update in two passes
    val newRecords = state.records.flatMap({ record =>
      record.element match {
        case e: MemElement =>
          // TODO fail gracefully if key does not exist
          instMap(OfModule(record.ofModule.get)).map { case (inst: Instance, ofMod: OfModule) =>
            nameSet.remove(e.name)
            val element = AbstractMacro(getUniqueName(e.name + "_" + ofMod.value), e.parent)
            renameMap.update(e.name, renameMap.getOrElse(e.name, Set()) ++ Set(element.name))
            FloorplanRecord(
              scope = record.scope,
              inst = record.inst.map(_ + "/" + inst.value),
              ofModule = Some(ofMod.value),
              element = element
            )
          }
        case _ => Seq(record)
      }
    }).map({ record =>
      record.element match {
        case e: MemElementArray =>
          val element = MemMacroArray(
            name = e.name,
            parent = e.parent,
            elements = e.elements.flatMap(x => renameMap(x)),
            width = e.width,
            height = e.height,
            area = e.area,
            aspectRatio = e.aspectRatio
          )
          FloorplanRecord(
            scope = record.scope,
            inst = record.inst, // should always be None
            ofModule = None,
            element = element
          )
        case _ => record
      }
    })
    state.copy(records = newRecords, level = 3) // TODO recalculate level?
  }
}

