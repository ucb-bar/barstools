// See LICENSE for license details.

package barstools.tapeout.transforms.pads

import firrtl.annotations._
import firrtl.ir._
import firrtl._
import firrtl.passes.Pass

// Analog is like UInt, SInt; it's not a direction (which is kind of weird)
// WARNING: Analog type is associated with Verilog InOut! i.e. even if digital pads are tri-statable, b/c tristate
// requires an additional ctrl signal, digital pads must be operated in a single "static" condition here; Analog will
// be paired with analog pads

class AddIOPads(pads: Seq[IOPad]) extends Pass {

  def name: String = "IOPads"

  def addIOPads(mod: Module): Seq[Module] = {
    // outside -> padframe -> internal
    // Top (with same name) contains 1) padframe + 2) internal signals
    val namespace = Namespace(mod)
    val padFrameName = namespace newName s"${mod.name}_PadFrame"
    val padFrame = buildPadFrame(mod.copy(name = padFrameName))
    val topInternalName = namespace newName s"${mod.name}_Internal"
    val padFrameInst = WDefInstance(NoInfo, padFrameName, padFrameName, UnknownType)
    val topInternalInst = WDefInstance(NoInfo, topInternalName, topInternalName, UnknownType)
    val padFrameRef = WRef(padFrameName, UnknownType, ExpKind, UNKNOWNGENDER)
    val topInternalRef = WRef(topInternalName, UnknownType, ExpKind, UNKNOWNGENDER)
    val connects = mod.ports.map { p => 
      val io = WRef(s"${p.name}", UnknownType, ExpKind, UNKNOWNGENDER)
      val intIo = WSubField(topInternalRef, s"${p.name}", UnknownType, UNKNOWNGENDER)
      val padFrameIntIo = WSubField(padFrameRef, s"${p.name}_Int", UnknownType, UNKNOWNGENDER)
      val padFrameExtIo = WSubField(padFrameRef, s"${p.name}_Ext", UnknownType, UNKNOWNGENDER)
      p.tpe match {
        case AnalogType(_) => 
          Seq(EmptyStmt)
          Seq(Attach(NoInfo, Seq(io, padFrameExtIo)), Attach(NoInfo, Seq(padFrameIntIo, intIo)))
        case _ => p.direction match {
          // If pad doesn't exist, hook up directly or fail?
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
    Seq(mod.copy(name = topInternalName), padFrame, mod.copy(body = Block(stmts)))
  }

  def buildPadFrame(mod: Module): Module = {
    // Internal = connection to original RTL; External = connection to outside world
    val intPorts = mod.ports map { p => p.copy(name = s"${p.name}_Int", direction = Utils.swap(p.direction)) }
    val extPorts = mod.ports map { p => p.copy(name = s"${p.name}_Ext") }
    // If no port annotation, just connect through (for debug purposes)
    val connects = mod.ports map { p => 
      val intRef = WRef(s"${p.name}_Int", UnknownType, ExpKind, UNKNOWNGENDER)
      val extRef = WRef(s"${p.name}_Ext", UnknownType, ExpKind, UNKNOWNGENDER)
      p.tpe match {
        case AnalogType(_) => 
          EmptyStmt
          //Attach(NoInfo, Seq(intRef, extRef))
        case _ =>
          val (lhs, rhs) = p.direction match {
            case Input => (intRef, extRef)
            case Output => (extRef, intRef)
          }
          Connect(NoInfo, lhs, rhs)
      }
    }    
    mod.copy(ports = intPorts ++ extPorts, body = Block(connects))
  }

  //val newReset = DefNode(NoInfo, "reset", DoPrim(Not, Seq(Reference("reset_n", Bool)), Seq.empty, Bool))
  
  def run(c: Circuit): Circuit =
    // TODO: Reparent! -- or not for testing consistency?
    // TODO: Allow top level to be a black box?
    c.copy(modules = c.modules.map {
      case mod: Module if mod.name == c.main => addIOPads(mod)
      case other => Seq(other)
    }.flatten)
}