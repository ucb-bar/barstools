package barstools.tapeout.transforms

import firrtl._
import firrtl.annotations._
import firrtl.passes._
import firrtl.ir._

object GetTargetDir {
  def apply(state: CircuitState): String = {
    val annos = state.annotations.getOrElse(AnnotationMap(Seq.empty)).annotations
    val destDir = annos.map {
      case Annotation(f, t, s) if t == classOf[transforms.BlackBoxSourceHelper] =>
        transforms.BlackBoxSource.parse(s) match {
          case Some(transforms.BlackBoxTargetDir(dest)) => Some(dest)
          case _ => None
        }
      case _ => None
    }.flatten
    val loc = {
      if (destDir.isEmpty) "."
      else destDir.head
    }
    val targetDir = new java.io.File(loc)
    if(!targetDir.exists()) FileUtils.makeDirectory(targetDir.getAbsolutePath)
    loc
  }
}

object WriteConfig {
  def apply(dir: String, file: String, contents: String): Unit = {
    val writer = new java.io.PrintWriter(new java.io.File(s"$dir/$file"))
    writer write contents
    writer.close()
  }
}