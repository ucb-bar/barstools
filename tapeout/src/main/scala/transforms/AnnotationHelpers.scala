package barstools.tapeout.transforms

import firrtl.ir._

abstract class SimpleAnnotation {
  def serialize: String
}

trait AnnotationSerializer {
  def outerSplit: String = ","
  def innerSplit: String = ":"
  def serialize: String = getClass.getDeclaredFields.map { f => 
    f.setAccessible(true)
    val name = f.getName
    val value = f.get(this) match {
      case b: SimpleAnnotation => a.serialize
      case n: FirrtlNode => n.serialize
      case s: String => s
      case i: java.lang.Integer => s"$i"
      case d: java.lang.Double => s"$d"
      case _ => throw new Exception("Invalid annotation value!")
    }
    Seq(annotationName, annotationValue).mkString(innerSplit)
  }.mkString(outerSplit)
}
