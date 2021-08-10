// See LICENSE for license details
package barstools.floorplan.hammer

import org.json4s._
import org.json4s.FieldSerializer.renameTo
import org.json4s.native.Serialization.{read, write, writePretty}
import org.json4s.JsonAST.JString
import org.json4s.ext.EnumNameSerializer

object HammerSerialization {

  /*
  // Custom serializer for PlacementType enums
  class PlacementTypeSerializer extends CustomSerializer[PlacementType.Value](format => (
    { case JString(s) => PlacementType.values.withName(s) },
    { case v: PlacementType.Value => JString(v.toString) }
  ))
  // Custom serializer for Orientation enums
  class OrientationSerializer extends CustomSerializer[Orientation.Value](format => (
    { case JString(s) => Orientation.values.withName(s) },
    { case v: Orientation.Value => JString(v.toString) }
  ))
  */

  val formats =
    DefaultFormats.skippingEmptyValues +
    new EnumNameSerializer(PlacementType) +
    new EnumNameSerializer(Orientation) +
    new EnumNameSerializer(ObstructionType) +
    FieldSerializer[PlacementConstraint](renameTo("typ", "type")) +
    FieldSerializer[HammerIR](renameTo("placementConstraints", "vlsi.inputs.placement_constraints"))

  def serialize(state: HammerIR): String = writePretty(state)(formats)

  def deserialize(str: String): HammerIR = {
    implicit val formats = HammerSerialization.formats
    read[HammerIR](str)
  }

}
