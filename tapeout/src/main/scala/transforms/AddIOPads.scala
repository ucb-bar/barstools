// See LICENSE for license details.

package barstools.tapeout.transforms

import chisel3.internal.InstanceId
import firrtl.annotations.{Annotation, CircuitName, ModuleName, Named}
import firrtl.ir._ //{Input, UIntType, IntWidth, Module, Port, DefNode, NoInfo, Reference, DoPrim, Block, Circuit}
import firrtl._
import firrtl.passes.Pass
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}

object HasIOPadsAnnotation {
  def apply(target: ModuleName, padTemplateFile: String): Annotation = Annotation(target, classOf[AddIOPadsTransform], padTemplateFile)
  def unapply(a: Annotation): Option[(Named, String)] = a match {
    case Annotation(m, t, padTemplateFile) if t == classOf[AddIOPadsTransform] => Some(m, padTemplateFile)
    case _ => None
  }
}

case class Pad(
    tpe: String,          // Digital, Fast Analog, Slow Analog, Vdd, Vss, Vddpst, Vsspst, etc.
    direction: String,    // Input, Output, InOut
    orientation: String,  // Vertical, Horizontal
    verilog: String
)

class AddIOPads(pads: Seq[Pad]) extends Pass {
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

class AddIOPadsTransform extends Transform with SimpleRun {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  import firrtl.passes._

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => CircuitState(state.circuit, LowForm)
      case Seq(HasIOPadsAnnotation(ModuleName(state.circuit.main, CircuitName(_)), padTemplateFile)) =>
        println(padTemplateFile)
        // TODO: Do I need to rerun resolve types, etc.?
        val passSeq = Seq(
          Legalize,
          new AddIOPads(Seq()),
          RemoveEmpty,
          CheckInitialization,
          InferTypes,
          Uniquify,
          ResolveKinds,
          ResolveGenders
        )
        CircuitState(runPasses(state.circuit, passSeq), LowForm)
      case annotations =>
        throw new Exception(s"There should be only one HasIOPads annotation: got ${annotations.mkString(" -- ")}")
    }
  }
}

trait HasIOPads {
  self: chisel3.Module =>
  // TODO: Submodule tapeout secret resources
  def createPads(component: InstanceId, padTemplateFile: String = "/ExamplePads.yaml"): Unit = {
    annotate(chisel3.experimental.ChiselAnnotation(component, classOf[AddIOPadsTransform], padTemplateFile))
  }
  createPads(this)
}