package barstools.tapeout.transforms.pads

import firrtl.annotations._
import firrtl._
import firrtl.ir._
import firrtl.transforms._

// Note: Not a pass b/c pass doesn't return Annotations (and we need to add more annotations)
object CreatePadBBs {
  // Get annotations for black box inlining of pad info + add black boxes for pads into circuit
  def apply(
      c: Circuit,
      topMod: String, 
      portPads: Seq[PortIOPad]): (Circuit, Seq[Annotation]) = {

    val (modsT, bbAnnosT) = c.modules.map {
      case mod: Module if mod.name == topMod => addPadBBs(mod, portPads, c)
      case other => (Seq(other), Seq())
    }.unzip

    val mods = modsT.flatten
    val bbAnnos = bbAnnosT.flatten
    (c.copy(modules = mods), bbAnnos)
  }

  def addPadBBs(mod: Module, portPads: Seq[PortIOPad], c: Circuit): (Seq[DefModule], Seq[Annotation]) = {
    // Only add unique pad modules
    val uniqueBBs = scala.collection.mutable.ArrayBuffer[PortIOPad]()
    mod.ports foreach { p => 
      val correspondingPad = portPads.filter(x => x.portName == p.name).head
      // L&R, T&B all use the same pads
      val fakeSide = correspondingPad.side match {
        case Left | Right => Left
        case Top | Bottom => Top
      }
      val correspondingPadNoName = correspondingPad.copy(portName = "", side = fakeSide)
      if (!uniqueBBs.contains(correspondingPadNoName)) {
        uniqueBBs += correspondingPadNoName
      }
    }

    // Analog pads only have 1 port ; create black boxes
    val bbs = uniqueBBs.map { x => 
      val ports = x.pad.tpe match {
        case "analog" => Seq(Port(NoInfo, "io", Input, AnalogType(IntWidth(1))))
        case "digital" => Seq(
          Port(NoInfo, "in", Input, UIntType(IntWidth(1))),
          Port(NoInfo, "out", Output, UIntType(IntWidth(1)))
        )
        case _ => throw new Exception("Pad must be analog/digital type!")
      }
      ExtModule(NoInfo, x.getPadName, ports, x.getPadName, Seq.empty)
    }.toSeq

    // Add annotations to black boxes to inline Verilog from template
    val annos = uniqueBBs.map { x => 
      BlackBoxSourceAnnotation(ModuleName(x.getPadName, CircuitName(c.main)), x.createPadInline)
    }.toSeq
    (bbs :+ mod, annos)
  }

}