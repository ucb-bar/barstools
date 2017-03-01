package barstools.tapeout.transforms.pads

import chisel3._

// NOTE: You can't really annotate outside of the module itself UNLESS you break up the compile step in 2 i.e.
// annotate post-Chisel but pre-Firrtl (unfortunate non-generator friendly downside). 
// It's recommended to have a Tapeout specific TopModule wrapper. 
// LIMITATION: All signals of a bus must be on the same chip side

// Chisel-y annotations
abstract class TopModule(
    modulePadAnnotation: ModulePadAnnotation = ModulePadAnnotation(),
    usePads: Boolean = true,
    override_clock: Option[Clock] = None, 
    override_reset: Option[Bool] = None) extends Module(override_clock, override_reset) {

  // TODO: Fix when Chisel has an alternative. Element = subclass of Data; get ground elements
  def extractElements(s: Data): Seq[Element] = {
    s match {
      case elt: Aggregate => elt.getElements flatMap {extractElements(_)}
      case elt: Element => Seq(elt)
      case _ => throw new Exception("Can't extract element from aggregate")
    }
  }

  val defaultPadSide = modulePadAnnotation.getDefaultPadSide

  // Annotate module as top module (that requires pad transform)
  // Specify the yaml file that indicates how pads are templated,
  // the default chip side that pads should be placed (if nothing is specified per IO),
  // and supply annotations: supply pad name, location, and #
  def createPads(): Unit = if (usePads) {
    annotate(TargetModulePadAnnoC(this, modulePadAnnotation).getAnno)
  }
    
  // Annotate IO with side + pad name
  def annotatePad(sig: Element, side: PadSide = defaultPadSide, name: String = ""): Unit = if (usePads) {
    val anno = IOPadAnnotation(side.serialize, name)
    annotate(TargetIOPadAnnoC(sig, anno).getAnno)
  }
  def annotatePad(sig: Aggregate, name: String): Unit = annotatePad(sig, side = defaultPadSide, name)
  def annotatePad(sig: Aggregate, side: PadSide): Unit = annotatePad(sig, side, name = "")
  def annotatePad(sig: Aggregate, side: PadSide, name: String): Unit = 
    extractElements(sig) foreach { x => annotatePad(x, side, name) }

  // There may be cases where pads were inserted elsewhere. If that's the case, allow certain IO to 
  // not have pads auto added. Note that annotatePad and noPad are mutually exclusive! 
  def noPad(sig: Element): Unit = if (usePads) annotate(TargetIOPadAnnoC(sig, NoIOPadAnnotation()).getAnno)
  def noPad(sig: Aggregate): Unit = extractElements(sig) foreach { x => noPad(x) }

  // Since this is a super class, this should be the first thing that gets run 
  // (at least when the module is actually at the top -- currently no guarantees otherwise :( firrtl limitation)
  createPads()
}