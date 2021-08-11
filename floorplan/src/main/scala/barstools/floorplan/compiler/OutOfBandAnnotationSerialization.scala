// See LICENSE for license details
package barstools.floorplan.compiler

import org.json4s._
import org.json4s.native.Serialization.{read, write, writePretty}

object OutOfBandAnnotationSerialization {

  val formats = DefaultFormats.skippingEmptyValues

  def serialize(seq: Seq[OutOfBandAnnotation]): String = writePretty(seq)(formats)

  def deserialize(str: String): Seq[OutOfBandAnnotation] = {
    implicit val formats = OutOfBandAnnotationSerialization.formats
    read[Seq[OutOfBandAnnotation]](str)
  }

}

