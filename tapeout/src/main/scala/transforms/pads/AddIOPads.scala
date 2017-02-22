// See LICENSE for license details.

package barstools.tapeout.transforms.pads

import firrtl.annotations._
import firrtl.ir._
import firrtl._
import firrtl.passes._

// Analog is like UInt, SInt; it's not a direction (which is kind of weird)
// WARNING: Analog type is associated with Verilog InOut! i.e. even if digital pads are tri-statable, b/c tristate
// requires an additional ctrl signal, digital pads must be operated in a single "static" condition here; Analog will
// be paired with analog pads

class AddIOPads(topMod: String, pads: Seq[PortIOPad]) extends Pass {

  def name: String = "Add Padframe"

  private def createInstance(name: String): WDefInstance = WDefInstance(NoInfo, name, name, UnknownType)

  def run(c: Circuit): Circuit = {
    val namespace = Namespace(c)
    val padFrameName = namespace newName s"${topMod}_PadFrame"
    val topInternalName = namespace newName s"${topMod}_Internal"
    // New modules consist of old modules (with top renamed to internal) + padFrame + newTop
    val newModsTemp = c.modules.map {
      case mod: Module if mod.name == topMod => {
        // Original top module is now internal module
        // Remove blackbox placeholder!
        def removeFakeBBPlaceholder(s: Statement): Seq[Statement] = s match {
          case b: Block => b.stmts.map(x => removeFakeBBPlaceholder(x)).flatten
          case WDefInstance(_, "fakeBBPlaceholder", _, _) => Seq(EmptyStmt)
          case _ => Seq(s)
        }
        val newStmts = removeFakeBBPlaceholder(mod.body)
        mod.copy(name = topInternalName, body = Block(newStmts))
      }
      case m => m
    } ++ Seq(buildPadFrame(padFrameName), buildTopWrapper(topInternalName, padFrameName))

    // Remove black box source helper placeholder module (doesn't do anything) -- hope Chisel kept the name
    val newMods = newModsTemp.filterNot(_.name == "FakeBBPlaceholder")

    // Reparent so circuit top is whatever uses pads!
    // TODO: Can the top level be a blackbox?
    c.copy(modules = newMods, main = topMod)
  }

  def buildTopWrapper(topInternalName: String, padFrameName: String): Module = {
    // outside -> padframe -> internal
    // Top (with same name) contains 1) padframe + 2) internal signals
    val padFrameInst = createInstance(padFrameName)
    val topInternalInst = createInstance(topInternalName)
    val padFrameRef = createRef(padFrameName)  
    val topInternalRef = createRef(topInternalName)
    val connects = pads.map { p => 
      val io = createRef(p.portName)
      val intIo = createSubField(topInternalRef, p.portName) 
      val padFrameIntIo = createSubField(padFrameRef, s"${p.portName}_Int")  
      val padFrameExtIo = createSubField(padFrameRef, s"${p.portName}_Ext")  
      p.port.tpe match {
        case AnalogType(_) => 
          // Analog pads only have 1 port
          Seq(Attach(NoInfo, Seq(io, padFrameExtIo)), Attach(NoInfo, Seq(io, intIo)))
        case _ => p.dir match {
          case Input => 
            // input to padframe ; padframe to internal
            Seq(Connect(NoInfo, padFrameExtIo, io), Connect(NoInfo, intIo, padFrameIntIo))
          case Output => 
            // internal to padframe ; padframe to output
            Seq(Connect(NoInfo, padFrameIntIo, intIo), Connect(NoInfo, io, padFrameExtIo))
        }
      }
    }.flatten 
    val stmts = Seq(padFrameInst, topInternalInst) ++ connects
    val ports = pads.map(p => p.port)
    Module(NoInfo, topMod, ports = ports, body = Block(stmts))
  }

  def buildPadFrame(padFrameName: String): Module = {
    // Internal = connection to original RTL; External = connection to outside world
    // Note that for analog pads, since there's only 1 port, only _Ext is used
    val intPorts = pads.map(p => p.port.tpe match {
      case AnalogType(_) => None
      case _ => Some(p.port.copy(name = s"${p.portName}_Int", direction = Utils.swap(p.dir)))
    }).flatten
    val extPorts = pads.map(p => p.port.copy(name = s"${p.portName}_Ext"))
    // Only create pad black boxes for ports that require them
    val padInsts = pads.filter(x => !x.pad.isEmpty).map(p => createInstance(p.firrtlBBName))
   
///////////////////////// TODO BELOW

    // Connect only if used
    // If no port annotation, just connect through (for debug purposes)
    val connects = pads map { p => 
      val intRef = createRef(s"${p.portName}_Int") 
      val extRef = createRef(s"${p.portName}_Ext") 
      p.port.tpe match {
        case AnalogType(_) => 
          EmptyStmt
          //Attach(NoInfo, Seq(intRef, extRef))
        case _ =>
          val (lhs, rhs) = p.dir match {
            case Input => (intRef, extRef)
            case Output => (extRef, intRef)
          }
          Connect(NoInfo, lhs, rhs)
      }
    }   
    Module(NoInfo, padFrameName, ports = intPorts ++ extPorts, body = Block(padInsts ++ connects))
  }

}

// don't connect nopad stuff -- just connect through for digital; nothing for analog
// check that pin is on right layer!!
// needs to convert between uint and sint types for bb interface
// add supply -- group by requirements ie 3 vdd together