// See LICENSE for license details.

package barstools.tapeout.transforms.clkgen

import chisel3._
import firrtl._
import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental._
import chisel3.iotesters._
import chisel3.util.HasBlackBoxInline

// Purely to see that clk src tagging works with BBs
class FakeBBClk extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val inClk = Input(Clock())
    val outClk = Output(Vec(3, Clock())) 
  })
  // Generates a "FakeBB.v" file with the following Verilog module
  setInline("FakeBBClk.v",
    s"""
      |module FakeBBClk(
      |  input inClk,
      |  output outClk_0,
      |  output outClk_1,
      |  output outClk_2
      |);
      |  always @* begin
      |    outClk_0 = inClk;
      |    outClk_1 = inClk;
      |    outClk_2 = inClk;      
      |  end
      |endmodule
    """.stripMargin)
}

class ModWithNestedClkIO(numPhases: Int) extends Bundle {
  val inClk = Input(Clock())
  val bbOutClk = Output(Vec(3, Clock()))
  val clkDivOut = Output(Vec(numPhases, Clock()))
}

class TestModWithNestedClkIO(numPhases: Int) extends Bundle {
  val bbOutClk = Output(Vec(3, Bool()))
  val clkDivOut = Output(Vec(numPhases, Bool()))
}

class ModWithNestedClk(divBy: Int, phases: Seq[Int]) extends Module {

  val io = IO(new ModWithNestedClkIO(phases.length))

  val bb = Module(new FakeBBClk)
  bb.io.inClk := io.inClk
  io.bbOutClk := bb.io.outClk
  val clkDiv = Module(new SEClkDivider(divBy, phases))
  clkDiv.io.reset := reset
  clkDiv.io.inClk := io.inClk
  phases.zipWithIndex.foreach { case (phase, idx) => io.clkDivOut(idx) := clkDiv.io.outClks(phase) }

}

class TopModuleWithClks(divBy: Int, phases: Seq[Int]) extends Module {
  val io = IO(new Bundle {
    val gen1 = new TestModWithNestedClkIO(phases.length)
    val gen2 = new TestModWithNestedClkIO(phases.length) 
    val gen3 = new TestModWithNestedClkIO(phases.length)
  })

  // Most complicated: test chain of clock generators
  val gen1 = Module(new ModWithNestedClk(divBy, phases))
  io.gen1.bbOutClk := Vec(gen1.io.bbOutClk.map(x => x.asUInt))
  io.gen1.clkDivOut := Vec(gen1.io.clkDivOut.map(x => x.asUInt))
  gen1.io.inClk := clock
  val gen2 = Module(new ModWithNestedClk(divBy, phases))
  io.gen2.bbOutClk := Vec(gen2.io.bbOutClk.map(x => x.asUInt))
  io.gen2.clkDivOut := Vec(gen2.io.clkDivOut.map(x => x.asUInt))
  gen2.io.inClk := gen1.io.clkDivOut.last
  val gen3 = Module(new ModWithNestedClk(divBy, phases))
  io.gen3.bbOutClk := Vec(gen3.io.bbOutClk.map(x => x.asUInt))
  io.gen3.clkDivOut := Vec(gen3.io.clkDivOut.map(x => x.asUInt))
  gen3.io.inClk := gen1.io.clkDivOut.last
}

class TopModuleWithClksTester(c: TopModuleWithClks) extends PeekPokeTester(c) {
  val numSubClkOutputs = c.io.gen1.clkDivOut.length
  val gen1Out = Seq.fill(numSubClkOutputs)(scala.collection.mutable.ArrayBuffer[String]())
  val gen2Out = Seq.fill(numSubClkOutputs)(scala.collection.mutable.ArrayBuffer[String]())
  val gen3Out = Seq.fill(numSubClkOutputs)(scala.collection.mutable.ArrayBuffer[String]())
  reset(10)
  for (t <- 0 until 30) {
    for (k <- 0 until numSubClkOutputs) {
      gen1Out(k) += peek(c.io.gen1.clkDivOut(k)).toString
      gen2Out(k) += peek(c.io.gen2.clkDivOut(k)).toString
      gen3Out(k) += peek(c.io.gen3.clkDivOut(k)).toString
    }
    step(1)
  }
  for (k <- 0 until numSubClkOutputs) {
    println(s"gen1Out($k): ${gen1Out(k)}")
    println(s"gen2Out($k): ${gen2Out(k)}")
    println(s"gen3Out($k): ${gen3Out(k)}")
  }  

}  

class ClkGenSpec extends FlatSpec with Matchers {

  def readOutputFile(dir: String, f: String): String = 
    scala.io.Source.fromFile(Seq(dir, f).mkString("/")).getLines.mkString("\n")
  def readResource(resource: String): String = {
    val stream = getClass.getResourceAsStream(resource)
    scala.io.Source.fromInputStream(stream).mkString
  }

  def checkOutputs(dir: String) = {
  }

  behavior of "top module with clk gens"

  it should "pass simple testbench" in {
    val optionsManager = new TesterOptionsManager {
      firrtlOptions = firrtlOptions.copy(
        compilerName = "verilog"
      )
      testerOptions = testerOptions.copy(isVerbose = true, backendName = "verilator", displayBase = 10)
      commonOptions = commonOptions.copy(targetDirName = "test_run_dir/ClkTB")
    }
    iotesters.Driver.execute(() => new TopModuleWithClks(4, Seq(0, 1, 3)), optionsManager) { c =>
      val dir = optionsManager.commonOptions.targetDirName
      checkOutputs(dir)   
      new TopModuleWithClksTester(c)
    } should be (true)
  }

}