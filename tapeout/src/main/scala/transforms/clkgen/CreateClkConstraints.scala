// See license file for details

package barstools.tapeout.transforms.clkgen

import firrtl.passes.clocklist._
import firrtl.annotations._
import firrtl.ir._
import firrtl.Utils._

// TODO: Really should be moved out of memlib
import firrtl.passes.memlib.AnalysisUtils._
import firrtl.passes._

// TODO: Wait until Albert merges into firrtl
import firrtl.analyses._

class CreateClkConstraints(
    clkModAnnos: Seq[TargetClkModAnnoF], 
    clkPortAnnos: Seq[TargetClkPortAnnoF],
    targetDir: String) extends Pass {

  def name = "Create clock constraints"
  
  def run(c: Circuit): Circuit = {

    val top = c.main
    
    // Remove everything from the circuit, unless it has a clock type
    // This simplifies the circuit drastically so InlineInstances doesn't take forever.
    val onlyClockCircuit = RemoveAllButClocks.run(c)

    val instanceGraph = new InstanceGraph(onlyClockCircuit)

    val clkModNames = clkModAnnos.map(x => x.targetName)
    val clkMods = clkModNames.map { x =>
      // NoDeDup was run so only 1 instance of each module should exist
      val inst = instanceGraph.findInstancesInHierarchy(x)
      require(inst.length == 1, "Clk modules should have not ben dedup-ed")
      // Return map of module name to absolute path as a string
      // Note: absolute path doesn't contain top module + to work with inlineInstances, 
      // delimit with $
      x -> inst.head.tail.map(x => x.name).mkString("$")
    }.toMap

    def getClks(tag: Option[Sink]): Map[String, String] = clkPortAnnos.map { 
      case TargetClkPortAnnoF(ComponentName(p, ModuleName(m, _)), ClkPortAnnotation(id, tag)) =>
        val absPath = Seq(clkMods(m), p).mkString(".")
        Some(absPath -> Seq(m, id).mkString("."))
      case _ => None
    }.flatten.toMap

    val clkSinks = getClks(Some(Sink()))
    val derivedClkSrcs = getClks(None)

    // Don't inline clock modules
    val modulesToInline = (c.modules.collect { 
      case Module(_, n, _, _) if n != top && !clkModNames.contains(n) => 
        ModuleName(n, CircuitName(c.main)) 
    }).toSet

    val inlineTransform = new InlineInstances
    val inlinedCircuit = inlineTransform.run(onlyClockCircuit, modulesToInline, Set()).circuit

    val topModule = inlinedCircuit.modules.find(_.name == top).getOrElse(throwInternalError)

    // Build a hashmap of connections to use for getOrigins
    val connects = getConnects(topModule)
 
    c
  }
}