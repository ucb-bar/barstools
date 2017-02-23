package barstools.tapeout.transforms.pads

import firrtl.annotations._
import firrtl._
import firrtl.ir._
import firrtl.passes._

case class PortIOPad(
    pad: Option[IOPad],
    side: PadSide,
    port: Port) {

  def portName = port.name
  def portWidth = bitWidth(port.tpe).intValue
  def dir = port.direction

  def padType(): String = pad match {
    case None => ""
    case Some(x) => x.tpe 
  }

  def orient(): PadOrientation = side match {
    case Left | Right => Horizontal
    case Top | Bottom => Vertical
  }
  def isDigital(): Boolean = padType == "digital"
  private def io(): String = padType match {
    case "digital" => 
      """|  input [WIDTH-1:0] in,
         |  output reg [WIDTH-1:0] out""".stripMargin
    case "analog" => "  inout [WIDTH-1:0] io"
    case "supply" | "" => ""
  }

  private def assignIO(): String = padType match {
    case "digital" => 
      """|    .in(in),
         |    .out(out)""".stripMargin
    case "analog" => "    .io(io)"
    case "supply" | "" => "" 
  }

  private def getPadVerilog(): String = pad match {
    case None => ""
    case Some(x) => x.createVerilog(dir, orient)
  }

  // Note: This includes both the pad wrapper + an additional wrapper for n-bit wide to 
  // multiple pad conversion!
  def createPadInline(): String = s"""inline\n${getPadName}_array.v\n${getPadVerilog}
module ${getPadName}_array #(
  parameter int WIDTH=1
)(
${io}
);
  ${getPadName} ${getPadName}[WIDTH-1:0](
${assignIO}
  );  
endmodule
""".stripMargin

  def getPadName(): String = pad match {
    case None => ""
    case Some(x) => x.getTemplateParams(dir, orient).name
  }

  def getPadArrayName(): String = pad match {
    case None => ""
    case Some(x) => x.getTemplateParams(dir, orient).arrayName
  }

  def firrtlBBName = s"${getPadArrayName}_${portName}"

}

// Note: Not a pass b/c pass doesn't return Annotations (and we need to add more annotations)
object AnnotatePortPads {

  def apply(
      c: Circuit,
      topMod: String, 
      pads: Seq[IOPad], 
      componentAnnos: Seq[PadAnnotation], 
      defaultSide: PadSide): Seq[PortIOPad] = {

    def lowerName(s: String): String = s.replace(".", "_").replace("[", "_")replace("]", "")

    def lowerAnnotations(): Seq[PadAnnotation] = {
      componentAnnos map { x => x.target match {
        case c: ComponentName => x.copy(target = c.copy(name = lowerName(c.name)))
        case _ => throw new Exception("Not a component annotation! Can't lower!")
      }}
    }

    // Make annotations match low form
    val annos = lowerAnnotations()

    def getPortIOPad(port: Port): PortIOPad = {
      val portAnnos = annos.filter(_.targetName == port.name)
      val portSideAnno = portAnnos.filter(_.tpe == PadSideAnno).map(x =>
        HasIOPadsAnnotation.getSideFromAnno(x.txt))
      require(portSideAnno.length <= 1, "No more than 1 side can be specified per port")
      // If no side specified, use module default
      val portSide = if (portSideAnno.length == 0) defaultSide else portSideAnno.head
      val portPadNameAnno = portAnnos.filter(_.tpe == PadNameAnno).map(x => x.txt)
      require(portPadNameAnno.length <= 1, "No more than 1 pad name can be specified per port")
      // Ports can only be digital or analog
      val portKind = port.tpe match {
        case AnalogType(_) => "analog"
        case e => "digital"
      }
      val validPadTypes = pads.filter(_.tpe == portKind)
      require(validPadTypes.length > 0, s"No $portKind pads specified in the config yaml file!")
      val usedPad = {
        // If no name specified, just use the first pad that matches the digital/analog type!
        if (portPadNameAnno.length == 0) validPadTypes.head
        else {
          // If a name match is found, use that! Otherwise, again use the first pad that matches the type!
          val possiblePadNameMatches = validPadTypes.filter(_.name == portPadNameAnno.head)
          if (possiblePadNameMatches.length == 0) validPadTypes.head
          else possiblePadNameMatches.head
        }
      }
      // Check if no pad is meant to be used
      val numNoPadAnnos = portAnnos.filter(_.tpe == NoPadAnno).length
      val noPad = numNoPadAnnos >= 1
      if (noPad) 
        require(
          portAnnos.length == numNoPadAnnos, 
          "Port should not have other pad annotations if no pads are to be used")
      PortIOPad(if (noPad) None else Some(usedPad), portSide, port)
    }

    // Rather than getting unique names, check for name space collision and error out
    val namespace = Namespace(c)
    pads.foreach { x => { 
      val testNames = Seq(
        x.getTemplateParams(Input, Horizontal).name,
        x.getTemplateParams(Input, Vertical).name,
        x.getTemplateParams(Output, Horizontal).name,
        x.getTemplateParams(Output, Vertical).name,
        // For Analog bit extraction black box (b/c it's not supported in firrtl)
        // In general, this seems cleaner than using FIRRTL to do any bit extraction/concatenation
        x.getTemplateParams(Input, Horizontal).arrayName,
        x.getTemplateParams(Input, Vertical).arrayName,
        x.getTemplateParams(Output, Horizontal).arrayName,
        x.getTemplateParams(Output, Vertical).arrayName
      ).distinct
      testNames.foreach { n => 
        require(!namespace.contains(n), "Pad name can't already be found in the circuit!")
      } 
    }}

    // Top MUST be internal module
    c.modules.filter(_.name == topMod).head.ports.map(x => getPortIOPad(x))
  }
}