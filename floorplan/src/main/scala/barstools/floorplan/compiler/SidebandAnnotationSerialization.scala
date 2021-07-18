// See LICENSE for license details
package barstools.floorplan.compiler

import org.json4s._
import org.json4s.native.Serialization.{read, write, writePretty}

object SidebandAnnotationSerialization {

  val formats = DefaultFormats.skippingEmptyValues

  def serialize(seq: Seq[SidebandAnnotation]): String = writePretty(seq)(formats)

  def deserialize(str: String): Seq[SidebandAnnotation] = {
    implicit val formats = SidebandAnnotationSerialization.formats
    read[Seq[SidebandAnnotation]](str)
  }

}

