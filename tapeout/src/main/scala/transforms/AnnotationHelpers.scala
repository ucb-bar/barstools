package barstools.tapeout.transforms

import firrtl.ir._

abstract class SimpleAnnotation {
  def serialize: String
}

abstract class AnnotationSerializer {
  def outerSplit: String = ","
  def innerSplit: String = ":"
  def serialize: String = getClass.getDeclaredFields.map { f => 
    f.setAccessible(true)
    val name = f.getName
    val value = f.get(this) match {
      case a: SimpleAnnotation => a.serialize
      case n: FirrtlNode => n.serialize
      case s: String => s
      case i: java.lang.Integer => s"$i"
      case d: java.lang.Double => s"$d"
      case e => throw new Exception(s"Invalid annotation value! ${e.getClass}")
    }
    Seq(name, value).mkString(innerSplit)
  }.mkString(outerSplit)
}
