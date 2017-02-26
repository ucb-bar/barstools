package barstools.tapeout.transforms.pads

import chisel3._
import chisel3.util.HasBlackBoxInline

// Chisel-y annotations
abstract class TopModule(
    padTemplateFile: String = "",
    defaultPadSide: PadSide = Top,
    supplyAnnos: Seq[SupplyAnnotation] = Seq.empty, 
    override_clock: Option[Clock] = None, 
    override_reset: Option[Bool] = None) extends Module(override_clock, override_reset) {

  // Annotate module as top module (that requires pad transform)
  // Specify the yaml file that indicates how pads are templated,
  // the default chip side that pads should be placed (if nothing is specified per IO),
  // and supply annotations: supply pad name, location, and #
  def createPads(mod: Module = this, 
      padTemplateFile: String = padTemplateFile, 
      defaultPadSide: PadSide = defaultPadSide,
      supplyAnnos: Seq[SupplyAnnotation] = supplyAnnos): Unit = {
    val anno = ModulePadAnnotation(padTemplateFile, defaultPadSide.serialize, supplyAnnos)
    annotate(TargetModulePadAnnoC(mod, anno).getAnno)
  }

  // TODO: Fix when Chisel has an alternative. Element = subclass of Data; get ground elements
  private def extractElements(s: Data): Seq[Element] = {
    s match {
      case elt: Aggregate => elt.getElements flatMap {extractElements(_)}
      case elt: Element => Seq(elt)
      case _ => throw new Exception("Can't extract element from aggregate")
    }
  }

  // Annotate IO with side + pad name
  def annotatePad(sig: Element, side: PadSide = defaultPadSide, name: String = ""): Unit = {
    val anno = IOPadAnnotation(side.serialize, name)
    annotate(TargetIOPadAnnoC(sig, anno).getAnno)
  }
  def annotatePad(sig: Aggregate, name: String): Unit = annotatePad(sig, side = defaultPadSide, name)
  def annotatePad(sig: Aggregate, side: PadSide): Unit = annotatePad(sig, side, name = "")
  def annotatePad(sig: Aggregate, side: PadSide, name: String): Unit = 
    extractElements(sig) foreach { x => annotatePad(x, side, name) } 

  // There may be cases where pads were inserted elsewhere. If that's the case, allow certain IO to 
  // not have pads auto added. Note that annotatePad and noPad are mutually exclusive! 
  def noPad(sig: Element): Unit = 
    annotate(TargetIOPadAnnoC(sig, NoIOPadAnnotation()).getAnno)
  def noPad(sig: Aggregate): Unit = 
    extractElements(sig) foreach { x => noPad(x) }

  // Since this is a super class, this should be the first thing that gets run 
  // (at least when the module is actually at the top -- currently no guarantees otherwise :( firrtl limitation)
  createPads()

  // Make sure that the BlackBoxHelper stuff is run (and after createPads to populate black box Verilog files)
  // by making a fake black box that should then be deleted
  private val fakeBBPlaceholder = FakeBBPlaceholder()
}

object FakeBBPlaceholder {
  def name = "FakeBBPlaceholder"
  def apply(): FakeBBPlaceholder = Module(new FakeBBPlaceholder)
}
private[barstools] class FakeBBPlaceholder extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle)
  setInline(s"${FakeBBPlaceholder.name}.v",
    s"""
    |module ${FakeBBPlaceholder.name}(
    |);
    |endmodule
    """.stripMargin)
}