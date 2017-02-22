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
      portPads: Seq[PortIOPad]): (Circuit, Seq[Annotation]) = {

    // Only add (parameterized) unique pad modules once to annotations
    val uniqueBBsNoName = scala.collection.mutable.ArrayBuffer[PortIOPad]()
    val uniqueBBsNameProto = scala.collection.mutable.ArrayBuffer[PortIOPad]()

    portPads foreach { correspondingPad => 
      // L&R, T&B all use the same pads
      val fakeSide = correspondingPad.side match {
        case Left | Right => Left
        case Top | Bottom => Top
      }
      // Analog is always InOut (not a Chisel thing)
      val fakeDir = if (correspondingPad.isDigital) correspondingPad.dir else Input
      // For uniqueness, don't care about everything that is overriden below
      val fakePad = correspondingPad.copy(portName = "", side = fakeSide, dir = fakeDir, portWidth = 1)
      // Only add pad if port needs pad
      if (!uniqueBBsNoName.contains(fakePad) && !correspondingPad.pad.isEmpty) {
        uniqueBBsNoName += fakePad
        uniqueBBsNameProto += correspondingPad
      }
    }

    val namespace = Namespace(c)
    // Note: Firrtl is silly and doesn't implement true parameterization -- each module with 
    // parameterization that potentially affects # of IO needs to be uniquely identified 
    // (but only in Firrtl)
    val bbs = portPads.map(x => x.pad match {
      // Don't add black box for port that doesn't require a pad
      case None => None
      case Some(_) => {
        val ports = x.padType match {
          case "analog" => Seq(Port(NoInfo, "io", Input, AnalogType(IntWidth(x.portWidth))))
          case "digital" => Seq(
            Port(NoInfo, "in", Input, UIntType(IntWidth(x.portWidth))),
            Port(NoInfo, "out", Output, UIntType(IntWidth(x.portWidth)))
          )
          case _ => throw new Exception("Port pad must be analog/digital type!")
        }
        val firrtlBBName = s"${x.getPadName}_array_${x.portName}"
        require(namespace tryName firrtlBBName, "ExtModule pad name can't already be found in the circuit!")
        Some(ExtModule(NoInfo, firrtlBBName, ports, s"${x.getPadName}_array", Seq(IntParam("WIDTH", x.portWidth))))
      }
    }).toSeq.flatten

    // Remove black box source helper placeholder module (doesn't do anything)
    val newMods = c.modules.filterNot(_.name == "FakeBBPlaceholder") ++ bbs

    // Add annotations to black boxes to inline Verilog from template
    // Again, note the weirdness in parameterization
    val annos = uniqueBBsNameProto.map { x => 
      val firrtlBBName = s"${x.getPadName}_array_${x.portName}"
      BlackBoxSourceAnnotation(ModuleName(firrtlBBName, CircuitName(c.main)), x.createPadInline)
    }.toSeq
    (c.copy(modules = newMods), annos)
  }

}