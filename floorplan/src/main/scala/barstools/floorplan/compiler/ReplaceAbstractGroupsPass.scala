// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

class ReplaceAbstractGroupsPass extends Pass {
  def execute(state: FloorplanState): FloorplanState = {
    val newRecords = state.records.map { record =>
      val newElement = record.element match {
        case e: WeightedGrid =>
          Some(ConstrainedWeightedGrid(
            name = e.name,
            parent = e.parent,
            xDim = e.xDim,
            yDim = e.yDim,
            elements = e.elements,
            xWeights = e.xWeights,
            yWeights = e.yWeights,
            packed = e.packed,
            width = Seq.fill(e.xDim*e.yDim) { Unconstrained() },
            height = Seq.fill(e.xDim*e.yDim) { Unconstrained() },
            area = Seq.fill(e.xDim*e.yDim) { Unconstrained() },
            aspectRatio = Seq.fill(e.xDim*e.yDim) { Unconstrained() }
          ))
        case e: ElasticGrid =>
          Some(ConstrainedElasticGrid(
            name = e.name,
            parent = e.parent,
            xDim = e.xDim,
            yDim = e.yDim,
            elements = e.elements,
            width = Seq.fill(e.xDim*e.yDim) { Unconstrained() },
            height = Seq.fill(e.xDim*e.yDim) { Unconstrained() },
            area = Seq.fill(e.xDim*e.yDim) { Unconstrained() },
            aspectRatio = Seq.fill(e.xDim*e.yDim) { Unconstrained() }
          ))
        case _ => None
      }
      newElement.map(e => record.copy(element = e)).getOrElse(record)
    }
    state.copy(records = newRecords)
  }
}
