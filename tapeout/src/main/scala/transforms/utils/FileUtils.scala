package barstools.tapeout.transforms

import firrtl.Parser.{IgnoreInfo, InfoMode}
import firrtl._
import firrtl.annotations._
import firrtl.passes._
import firrtl.ir._

import scala.io.Source
import scala.util.control.ControlThrowable

object WriteConfig {
  def apply(dir: String, file: String, contents: String): Unit = {
    val writer = new java.io.PrintWriter(new java.io.File(s"$dir/$file"))
    writer write contents
    writer.close()
  }
}

object GetTargetDir {
  def apply(state: CircuitState): String = {
    val annos = state.annotations
    val destDir = annos.collect {
      case firrtl.transforms.BlackBoxTargetDirAnno(dest) => dest
      //case Annotation(f, t, s) if t == classOf[transforms.BlackBoxSourceHelper] =>
      //  transforms.BlackBoxSource.parse(s) match {
      //    case Some(transforms.BlackBoxTargetDir(dest)) => Some(dest)
      //    case _ => None
      //  }
      //case _ => None
    }
    val loc = {
      if (destDir.isEmpty) "."
      else destDir.head
    }
    val targetDir = new java.io.File(loc)
    if(!targetDir.exists()) FileUtils.makeDirectory(targetDir.getAbsolutePath)
    loc
  }
}

// Fake transform just to track Technology information directory
/*
object TechnologyLocation {
  def apply(dir: String): Annotation = {
    Annotation(CircuitName("All"), classOf[TechnologyLocation], dir)
  }
}
class TechnologyLocation extends Transform {
  def inputForm: CircuitForm = LowForm
  def outputForm: CircuitForm = LowForm
  def execute(state: CircuitState) = throw new Exception("Technology Location transform execution doesn't work!")
  def get(state: CircuitState): String = {
    val annos = state.annotations.underlying
    val dir = annos.map {
      case Annotation(f, t, s) if t == classOf[TechnologyLocation] => Some(s)
      case _ => None
    }.flatten
    dir.length match {
      case 0 => ""
      case 1 => 
        val targetDir = new java.io.File(dir.head)
        if(!targetDir.exists()) throw new Exception("Technology yaml directory doesn't exist!")
        dir.head
      case _ => throw new Exception("Only 1 tech directory annotation allowed!")
    }
  }
}
*/

object Driver {
  def compile(
      input: String,
      output: String,
      compiler: Compiler,
      infoMode: InfoMode = IgnoreInfo,
      customTransforms: Seq[Transform] = Seq.empty,
      annotations: AnnotationSeq = AnnotationSeq(Seq.empty)
  ): String = {
      val outputBuffer = new java.io.CharArrayWriter
      try {
          val parsedInput = Parser.parse(Source.fromFile(input).getLines(), infoMode)
          compiler.compile(
             CircuitState(parsedInput, ChirrtlForm, annotations),
             outputBuffer,
             customTransforms)
        }

        catch {
          // Rethrow the exceptions which are expected or due to the runtime environment (out of memory, stack overflow)
          case p: ControlThrowable => throw p
          case p: PassException  => throw p
          case p: FIRRTLException => throw p
           // Treat remaining exceptions as internal errors.
             case e: Exception => firrtl.Utils.throwInternalError(exception = Some(e))
        }

        val outputFile = new java.io.PrintWriter(output)
      val outputString = outputBuffer.toString
      outputFile.write(outputString)
      outputFile.close()
      outputString
    }
}


