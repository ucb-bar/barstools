// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._
import scala.collection.{Seq, Map}

case class OutOfBandAnnotation(
  ofModule: String,
  width: Option[BigDecimal] = None,
  height: Option[BigDecimal] = None,
  area: Option[BigDecimal] = None
) {
  def ++(that: OutOfBandAnnotation): OutOfBandAnnotation = {
    assert(this.ofModule == that.ofModule)
    assert(!this.width.isDefined || !that.width.isDefined)
    assert(!this.height.isDefined || !that.height.isDefined)
    assert(!this.area.isDefined || !that.area.isDefined)
    OutOfBandAnnotation(
      this.ofModule,
      this.width.orElse(that.width),
      this.height.orElse(that.width),
      this.area.orElse(that.area)
    )
  }

  def widthConstraint = width.map(x => EqualTo(x)).getOrElse(Unconstrained())
  def heightConstraint = height.map(x => EqualTo(x)).getOrElse(Unconstrained())
  def areaConstraint = area.map(x => EqualTo(x)).getOrElse(Unconstrained())
}

object OutOfBandAnnotationMap {
  def fromFiles(files: Seq[String]): Map[String, OutOfBandAnnotation] = fromSeq(OutOfBandAnnotationSeq.fromFiles(files))
  def fromFile(file: String): Map[String, OutOfBandAnnotation] = fromSeq(OutOfBandAnnotationSeq.fromFile(file))
  def fromSeq(seq: Seq[OutOfBandAnnotation]): Map[String, OutOfBandAnnotation] = seq.groupBy(_.ofModule).mapValues(_.reduce(_ ++ _))
}

object OutOfBandAnnotationSeq {
  def fromFiles(files: Seq[String]): Seq[OutOfBandAnnotation] = files.flatMap(x => fromFile(x))
  def fromFile(file: String): Seq[OutOfBandAnnotation] = {
    val source = scala.io.Source.fromFile(file)
    val annos = OutOfBandAnnotationSerialization.deserialize(source.getLines.mkString("\n"))
    source.close()
    annos
  }
}

