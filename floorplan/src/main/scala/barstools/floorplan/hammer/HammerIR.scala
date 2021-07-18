// See LICENSE for license details
package barstools.floorplan.hammer

import barstools.floorplan._

import java.io.{Writer}
import scala.math.{BigInt, BigDecimal}

// This implements a small subset of HammerIR

object HammerIR {

  def fromFloorplanState(state: FloorplanState): HammerIR = {
    assert(state.level == 0, "Can only convert level 0 FloorplanState")
    val placementConstraints = state.records.map({ record =>
      record.element match {
        case c: PlacedMacro =>
          Some(PlacementConstraint(
            path = record.fullPath,
            typ = PlacementType.hardmacro,
            orientation = Orientation.r0, // TODO represent this in FPIR
            x = toMicrons(c.x),
            y = toMicrons(c.y),
            create_physical = Some(false),
            width = toMicrons(c.width),
            height = toMicrons(c.height),
            master = record.ofModule,
            margins = None,
            top_layer = None, // TODO need this for macros
            layers = None, // TODO need this for macros
            obs_types = None // TODO need this for macros
          ))
        case c: PlacedLogicRect =>
          Some(PlacementConstraint(
            path = record.fullPath,
            typ = PlacementType.placement,
            orientation = Orientation.r0, // TODO represent this in FPIR
            x = toMicrons(c.x),
            y = toMicrons(c.y),
            create_physical = Some(false),
            width = toMicrons(c.width),
            height = toMicrons(c.height),
            master = None,
            margins = None,
            top_layer = None,
            layers = None,
            obs_types = None
          ))
        case c: PlacedHierarchicalTop =>
          Some(PlacementConstraint(
            path = record.fullPath,
            typ = PlacementType.placement,
            orientation = Orientation.r0,
            x = toMicrons(c.x),
            y = toMicrons(c.y),
            create_physical = Some(false),
            width = toMicrons(c.width),
            height = toMicrons(c.height),
            master = None,
            margins = Some(PlacementMargins(c.margins)),
            top_layer = None,
            layers = None,
            obs_types = None
          ))
        case c => None
      }
    }).filter(_.isDefined).map(_.get)
    HammerIR(placementConstraints = placementConstraints)
  }

  def serialize(h: HammerIR): String = HammerSerialization.serialize(h)
  def deserialize(s: String): HammerIR = HammerSerialization.deserialize(s)
  def toMicrons(l: BigDecimal): Double = l.toDouble
}

final case class HammerIR private[hammer] (
  placementConstraints: Seq[PlacementConstraint]
)

final case class PlacementConstraint private[hammer] (
  path: String,
  typ: PlacementType.Value, // type is a keyword, so we use typ and rename it in the serializer
  orientation: Orientation.Value,
  // TODO these should ideally all be decimal types, not doubles
  x: Double,
  y: Double,
  width: Double,
  height: Double,
  // End TODO
  master: Option[String],
  create_physical: Option[Boolean],
  margins: Option[PlacementMargins],
  top_layer: Option[String],
  layers: Option[Seq[String]],
  obs_types: Option[Seq[ObstructionType.Value]]
)

object PlacementType extends Enumeration {
  val dummy, placement, toplevel, hardmacro, hierarchical, obstruction = Value
}

object Orientation extends Enumeration {
  val r0, r90, r180, r270, mx, mx90, my, my90 = Value
}

object ObstructionType extends Enumeration {
  val place, route, power = Value
}

final case class PlacementMargins private[hammer] (
  // TODO these should ideally all be decimal types, not doubles
  left: Double,
  right: Double,
  top: Double,
  bottom: Double
)

object PlacementMargins {
  def apply(m: barstools.floorplan.Margins): PlacementMargins = PlacementMargins(
    left=m.left.toDouble,
    right=m.right.toDouble,
    top=m.top.toDouble,
    bottom=m.bottom.toDouble
  )
}

