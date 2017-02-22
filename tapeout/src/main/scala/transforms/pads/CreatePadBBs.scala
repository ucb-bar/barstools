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

    // Only add pad if port needs pad
    val usedPortPads = portPads.filter(x => !x.pad.isEmpty)

    usedPortPads foreach { correspondingPad => 
      // L&R, T&B all use the same pads
      val fakeSide = correspondingPad.side match {
        case Left | Right => Left
        case Top | Bottom => Top
      }
      // Analog is always InOut (not a Chisel thing)
      val fakeDir = if (correspondingPad.isDigital) correspondingPad.dir else Input
      val fakePort = Port(NoInfo, "", fakeDir, UIntType(IntWidth(1)))
      // For uniqueness, don't care about everything that is overriden below
      val fakePad = correspondingPad.copy(side = fakeSide, port = fakePort)
      if (!uniqueBBsNoName.contains(fakePad)) {
        uniqueBBsNoName += fakePad
        uniqueBBsNameProto += correspondingPad
      }
    }

    val namespace = Namespace(c)
    // Note: Firrtl is silly and doesn't implement true parameterization -- each module with 
    // parameterization that potentially affects # of IO needs to be uniquely identified 
    // (but only in Firrtl)
    val bbs = usedPortPads.map(x => {
      val ports = x.padType match {
        case "analog" => Seq(Port(NoInfo, "io", Input, AnalogType(IntWidth(x.portWidth))))
        case "digital" => Seq(
          Port(NoInfo, "in", Input, UIntType(IntWidth(x.portWidth))),
          Port(NoInfo, "out", Output, UIntType(IntWidth(x.portWidth)))
        )
        case _ => throw new Exception("Port pad must be analog/digital type!")
      }
      require(namespace tryName x.firrtlBBName, "ExtModule pad name can't already be found in the circuit!")
      Some(ExtModule(NoInfo, x.firrtlBBName, ports, x.getPadArrayName, Seq(IntParam("WIDTH", x.portWidth))))
    } ).toSeq.flatten

    val newMods = c.modules ++ bbs

    // Add annotations to black boxes to inline Verilog from template
    // Again, note the weirdness in parameterization
    val annos = uniqueBBsNameProto.map { x => 
      Annotation(ModuleName(x.firrtlBBName, CircuitName(c.main)), classOf[BlackBoxSourceHelper], x.createPadInline)
    }.toSeq
    (c.copy(modules = newMods), annos)
  }

}