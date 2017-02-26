package barstools.tapeout.transforms.pads

import firrtl.annotations._
import firrtl._
import firrtl.ir._
import firrtl.passes._

case class TopSupplyPad(
    pad: ChipPad,
    padSide: PadSide,
    num: Int
) {
  def padType = pad.padType
  require(pad.padType == SupplyPad)

  def padOrientation = padSide.orientation
  def getPadName = pad.getName(NoDirection, padOrientation)
  def firrtlBBName = getPadName

  def createPadInline(): String = {
    def getPadVerilog(): String = pad.getVerilog(NoDirection, padOrientation)
    s"""inline
      |${getPadName}.v
      |${getPadVerilog}""".stripMargin
  }
}

object AnnotateSupplyPads {
  def apply(
      pads: Seq[ChipPad], 
      supplyAnnos: Seq[SupplyAnnotation]
  ): Seq[TopSupplyPad] = {
    supplyAnnos.map( a => 
      pads.find(_.name == a.padName) match {
        case None => 
          throw new Exception(s"Supply pad ${a.padName} not found in Yaml file!")
        case Some(x) => 
          Seq(
            TopSupplyPad(x, Left, a.leftSide),
            TopSupplyPad(x, Right, a.rightSide),
            TopSupplyPad(x, Top, a.topSide),
            TopSupplyPad(x, Bottom, a.bottomSide))
      }
    ).flatten.filter(_.num > 0)
  }
}