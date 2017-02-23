// See LICENSE for license details.

package barstools.tapeout.transforms.pads

import chisel3._
import firrtl._
import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental._
import chisel3.util.HasBlackBoxInline
import chisel3.iotesters._

class BB extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val c = Input(SInt(14.W))
    val z = Output(SInt(16.W))
    val analog1 = Analog(3.W)
    val analog2 = analog1.chiselCloneType  
  })
  // Generates a "FakeBB.v" file with the following Verilog module
  setInline("FakeBB.v",
    s"""
      |module BB(
      |  input [15:0] c,
      |  output [15:0] z,
      |  inout [2:0] analog1,
      |  inout [2:0] analog2
      |);
      |  always @* begin
      |    z = 2 * c;
      |    analog2 = analog1 + 1;
      |  end
      |endmodule
    """.stripMargin)
}

// If no template file is provided, it'll use the default one (example) in the resource folder
// Default pad side is Top if no side is specified for a given IO
class ExampleTopModuleWithBB extends TopModule(padTemplateFile = "", defaultPadSide = Top) {
  val io = IO(new Bundle {
    val a = Input(UInt(15.W))
    val b = a.chiselCloneType
    val c = Input(SInt(14.W))
    val x = Output(UInt(16.W))
    val y = x.chiselCloneType
    val z = Output(SInt(16.W))
    val analog1 = Analog(3.W)
    val analog2 = analog1.chiselCloneType
    val v = Output(Vec(3, UInt(5.W)))
  })

  // Can annotate aggregates with pad side location + pad name (should be a name in the yaml template)
  annotatePad(io.v, Right, "easy_digital")
  // Can annotate individual elements
  annotatePad(io.analog1, Left, "fast_custom")
  annotatePad(io.analog2, Bottom, "slow_foundry")
  // Looks for a pad that matches the IO type (digital in, digital out, analog) if no name is specified
  Seq(io.a, io.b, io.c, io.x) foreach { x => annotatePad(x, Left) }
  // Some signals might not want pads associated with them
  noPad(io.y)
  // Clk might come directly from bump
  noPad(clock)

  val bb = Module(new BB())
  bb.io.c := io.c
  io.z := bb.io.z
  bb.io.analog1 <> io.analog1
  bb.io.analog2 <> io.analog2

  io.x := io.a + 1.U
  io.y := io.b - 1.U

  io.v foreach { lhs => lhs := io.a }
  
}

class SimpleTopModuleTester(c: ExampleTopModuleWithBB) extends PeekPokeTester(c) {
  val ax = Seq(5, 3)
  val bx = Seq(8, 2)
  val cx = Seq(-11, -9)
  for (i <- 0 until ax.length) {
    poke(c.io.a, ax(i))
    poke(c.io.b, bx(i))
    poke(c.io.c, cx(i))
    expect(c.io.x, ax(i) + 1)
    expect(c.io.y, bx(i) - 1)
    expect(c.io.z, 2 * cx(i))
    c.io.v foreach {out => expect(out, ax(i))}
  }
  // Analog can't be peeked + poked 
}  

class IOPadSpec extends FlatSpec with Matchers {

  behavior of "top module with blackbox"

  it should "pass simple testbench" in {
    val optionsManager = new TesterOptionsManager {
      firrtlOptions = firrtlOptions.copy(compilerName = "verilog")
      testerOptions = testerOptions.copy(isVerbose = true, backendName = "verilator", displayBase = 10)
    }
    iotesters.Driver.execute(() => new ExampleTopModuleWithBB, optionsManager) { c =>
      new SimpleTopModuleTester(c)
    } should be (true)
  }

  it should "create proper IO pads + black box in low firrtl" in {
    val optionsManager = new ExecutionOptionsManager("barstools") with HasChiselExecutionOptions with HasFirrtlOptions {
      firrtlOptions = firrtlOptions.copy(compilerName = "low")
      commonOptions = commonOptions.copy(targetDirName = "test_run_dir/LoFirrtl")
      //commonOptions = commonOptions.copy(globalLogLevel = logger.LogLevel.Info)
    }
    val success = chisel3.Driver.execute(optionsManager, () => new ExampleTopModuleWithBB) match {
      case ChiselExecutionSuccess(_, chirrtl, Some(FirrtlExecutionSuccess(_, firrtl))) =>
        firrtl should include ("_PadFrame") 
        firrtl should include ("_Internal") 
        true
      case _ => false
    } 
    success should be (true)
  } 

  it should "create proper IO pads + black box in verilog" in {
    val optionsManager = new ExecutionOptionsManager("barstools") with HasChiselExecutionOptions with HasFirrtlOptions {
      firrtlOptions = firrtlOptions.copy(compilerName = "verilog")
      commonOptions = commonOptions.copy(targetDirName = "test_run_dir/Verilog")
      //commonOptions = commonOptions.copy(globalLogLevel = logger.LogLevel.Info)
    }
    val success = chisel3.Driver.execute(optionsManager, () => new ExampleTopModuleWithBB) match {
      case ChiselExecutionSuccess(_, chirrtl, Some(FirrtlExecutionSuccess(_, verilog))) => 
        true
      case _ => false
    } 
    success should be (true)
  } 

}