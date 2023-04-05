package barstools.tapeout.transforms

import chisel3.util.Queue
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.TestSuite
import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.stage.RunFirrtlTransformAnnotation

import java.io.File
//import chiseltest._
import firrtl._
import firrtl.options.TargetDirAnnotation


trait CompilerTest { this: TestSuite =>
  protected def annos: AnnotationSeq = Seq()

  protected def compile[M <: Module](gen: => M, target: String, a: AnnotationSeq = List(), ll: String = "warn"): (String, AnnotationSeq) = {
    val stage = new ChiselStage

    // ensure that test files don't just end up in the root directory
    val testName = this.suiteName //sanitizeFileName(scalaTestContext.value.get.name)
    val testRunDir = TargetDirAnnotation("test_run_dir" + File.separator + testName)

    val r = stage.execute(Array("-X", target, "-ll", ll), ChiselGeneratorAnnotation(() => gen) +: testRunDir +: a ++: annos)
    val src = r.collect {
      case EmittedFirrtlCircuitAnnotation(a) => a
      case EmittedFirrtlModuleAnnotation(a) => a
      case EmittedVerilogCircuitAnnotation(a) => a
      case EmittedVerilogModuleAnnotation(a) => a }.map(_.value).mkString("")
    (src, r)
  }
}

class SplitTileSpec extends AnyFreeSpec with CompilerTest {
  class DummyChipTop extends Module {

  }

  "thing" in {
    val (firrtl, annos) = compile(new Queue(UInt(8.W), 10), "low", a = Seq(RunFirrtlTransformAnnotation(new SplitTileTransforms)))
    println(firrtl)
  }
}
