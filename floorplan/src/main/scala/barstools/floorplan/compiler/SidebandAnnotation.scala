// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._
import scala.collection.{Seq, Map}

case class SidebandAnnotation(
  ofModule: String,
  width: Option[BigDecimal] = None,
  height: Option[BigDecimal] = None,
  area: Option[BigDecimal] = None
) {
  def ++(that: SidebandAnnotation): SidebandAnnotation = {
    assert(this.ofModule == that.ofModule)
    assert(!this.width.isDefined || !that.width.isDefined)
    assert(!this.height.isDefined || !that.height.isDefined)
    assert(!this.area.isDefined || !that.area.isDefined)
    SidebandAnnotation(
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

object SidebandAnnotationMap {
  def fromFiles(files: Seq[String]): Map[String, SidebandAnnotation] = fromSeq(SidebandAnnotationSeq.fromFiles(files))
  def fromFile(file: String): Map[String, SidebandAnnotation] = fromSeq(SidebandAnnotationSeq.fromFile(file))
  def fromSeq(seq: Seq[SidebandAnnotation]): Map[String, SidebandAnnotation] = seq.groupBy(_.ofModule).mapValues(_.reduce(_ ++ _))
}

object SidebandAnnotationSeq {
  def fromFiles(files: Seq[String]): Seq[SidebandAnnotation] = files.flatMap(x => fromFile(x))
  def fromFile(file: String): Seq[SidebandAnnotation] = {
    val source = scala.io.Source.fromFile(file)
    val annos = SidebandAnnotationSerialization.deserialize(source.getLines.mkString("\n"))
    source.close()
    annos
  }
}

