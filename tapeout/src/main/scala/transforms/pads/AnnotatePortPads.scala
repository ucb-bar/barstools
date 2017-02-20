package barstools.tapeout.transforms.pads

import firrtl.annotations._
import firrtl._
import firrtl.ir._

case class PortIOPad(
    portName: String,
    pad: IOPad,
    side: PadSide,
    dir: Direction) {
  def orient(): PadOrientation = side match {
    case Left | Right => Horizontal
    case Top | Bottom => Vertical
  }
  def createVerilog(): String = pad.createVerilog(dir, orient)
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

    def getPortIOPad(port: Port): PortIOPad = {
      val annos = lowerAnnotations()
      val portAnnos = annos.filter { _.targetName == port.name } 
      val portSideAnno = portAnnos.filter { _.tpe == PadSideAnno }.map { x =>
        HasIOPadsAnnotation.getSideFromAnno(x.txt) }
      require(portSideAnno.length <= 1, "No more than 1 side can be specified per port")
      // If no side specified, use module default
      val portSide = if (portSideAnno.length == 0) defaultSide else portSideAnno.head
      val portPadNameAnno = portAnnos.filter { _.tpe == PadNameAnno}.map { x => x.txt }
      require(portPadNameAnno.length <= 1, "No more than 1 pad name can be specified per port")
      // Ports can only be digital or analog
      val portKind = port.tpe match {
        case AnalogType(_) => "analog"
        case e => "digital"
      }
      val validPadTypes = pads.filter { _.tpe == portKind }
      require(validPadTypes.length > 0, s"No $portKind pads specified in the config yaml file!")
      val usedPad = {
        // If no name specified, just use the first pad that matches the digital/analog type!
        if (portPadNameAnno.length == 0) validPadTypes.head
        else {
          // If a name match is found, use that! Otherwise, again use the first pad that matches the type!
          val possiblePadNameMatches = validPadTypes.filter { _.name == portPadNameAnno.head }
          if (possiblePadNameMatches.length == 0) validPadTypes.head
          else possiblePadNameMatches.head
        }
      }
      PortIOPad(port.name, usedPad, portSide, port.direction)
    }

    // Top MUST be internal module
    c.modules.map {
      case mod: Module if mod.name == topMod => {
        mod.ports map { x => getPortIOPad(x) }
      }
      case other => Seq()
    }.flatten

  }
}