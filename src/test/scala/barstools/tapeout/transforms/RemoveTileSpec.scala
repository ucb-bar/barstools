package barstools.tapeout.transforms

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.TestSuite
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl._
import firrtl.options.TargetDirAnnotation
import java.io.File


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

class RemoveTileSpec extends AnyFreeSpec with CompilerTest {
  class DummyTestHarness extends Module {
    val io = IO(new Bundle {
      val success = Output(Bool())
    })

    val chiptop = Module(new DummyChipTop())
    io.success := (chiptop.io.ct_master === chiptop.io.other_port2)

    val cnt = RegInit(0.U(64.W))
    cnt := cnt + 1.U
    chiptop.io.ct_slave := cnt
    chiptop.io.other_port := cnt + 1.U
  }

  class DummyChipTop extends Module {
    val io = IO(new Bundle {
      val ct_slave = Input(UInt(64.W))
      val ct_master = Output(UInt(64.W))
      val other_port = Input(UInt(64.W))
      val other_port2 = Output(UInt(64.W))
    })

    val tile = Module(new RocketTile())

    io.ct_master := tile.io.master
    tile.io.slave := io.ct_slave

    io.other_port2 := io.other_port + 1.U
  }

  class RocketTile extends Module {
    val io = IO(new Bundle {
      val master = Output(UInt(64.W))
      val slave = Input(UInt(64.W))
    })

    val prev_slave = RegInit(0.U(64.W))

    prev_slave := io.slave
    io.master := prev_slave
  }

  "RemoveTileSpec" in {
    val (firrtl, annos) = compile(new RocketTile, "low", a = Seq(RunFirrtlTransformAnnotation(new RemoveTileTransforms)))
    println(firrtl)
    annos.toSeq.foreach(println)
  }
}
