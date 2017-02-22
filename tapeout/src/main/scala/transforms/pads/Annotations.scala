package barstools.tapeout.transforms.pads

import chisel3.internal.InstanceId
import chisel3.experimental._
import firrtl.annotations._
import chisel3._
import chisel3.util.HasBlackBoxInline

trait PadSide
object Left extends PadSide
object Right extends PadSide
object Top extends PadSide
object Bottom extends PadSide

abstract class TopModule(
    padTemplateFile: String = "",
    defaultPadSide: PadSide = Top,
    override_clock: Option[Clock] = None, 
    override_reset: Option[Bool] = None) extends Module(override_clock, override_reset) {
  // Annotate module as top module (that requires pad transform)
  def createPads(mod: InstanceId, padTemplateFile: String, defaultPadSide: PadSide): Unit = {
    // Pad information
    annotate(
      ChiselAnnotation(mod, classOf[AddIOPadsTransform], HasIOPadsAnnotation.genTemplateFileAnno(padTemplateFile)))
    // Default pad side
    annotate(ChiselAnnotation(mod, classOf[AddIOPadsTransform], HasIOPadsAnnotation.genSideAnno(defaultPadSide)))
  }

  // Element = subclass of Data; get ground elements
  private def extractElements(s: Data): Seq[Element] = {
    s match {
      case elt: Aggregate => elt.getElements flatMap {extractElements(_)}
      case elt: Element => Seq(elt)
      case _ => throw new Exception("Can't extract element from aggregate")
    }
  }

  // Chisel annotations (different from Firrtl!)
  // Annotate IO with side + pad name
  def annotatePad(sig: InstanceId, name: String): Unit = 
    annotate(ChiselAnnotation(sig, classOf[AddIOPadsTransform], HasIOPadsAnnotation.genPadNameAnno(name)))
  def annotatePad(sig: InstanceId, side: PadSide): Unit = 
    annotate(ChiselAnnotation(sig, classOf[AddIOPadsTransform], HasIOPadsAnnotation.genSideAnno(side)))
  def annotatePad(sig: InstanceId, side: PadSide, name: String): Unit = {
    annotatePad(sig, name)
    annotatePad(sig, side)
  }

  def annotatePad(sig: Aggregate, name: String): Unit = 
    extractElements(sig) foreach { x => annotatePad(x, name) }
  def annotatePad(sig: Aggregate, side: PadSide): Unit = 
    extractElements(sig) foreach { x => annotatePad(x, side) }
  def annotatePad(sig: Aggregate, side: PadSide, name: String): Unit = 
    extractElements(sig) foreach { x => annotatePad(x, side, name) } 

  // There may be cases where pads were inserted elsewhere. If that's the case, allow certain IO to 
  // not have pads auto added. 
  def noPad(sig: InstanceId): Unit = 
    annotate(ChiselAnnotation(sig, classOf[AddIOPadsTransform], HasIOPadsAnnotation.noPadAnno()))
  def noPad(sig: Aggregate): Unit = 
    extractElements(sig) foreach { x => noPad(x) }

  // Since this is a super class, this should be the first thing that gets run 
  // (at least when the module is actually at the top -- currently no guarantees otherwise :( firrtl limitation)
  createPads(this, padTemplateFile, defaultPadSide)

  // Make sure that the BlackBoxHelper stuff is run (and after createPads to populate black box Verilog files)
  // by making a fake black box that should then be deleted
  val fakeBBPlaceholder = Module(new FakeBBPlaceholder)
  class FakeBBPlaceholder extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle)
    setInline("FakeBBPlaceholder.v",
      s"""
        |module FakeBBPlaceholder(
        |);
        |endmodule
      """.stripMargin)
  }

}

trait PadAnnotationType
object PadSideAnno extends PadAnnotationType
object PadNameAnno extends PadAnnotationType
object PadTemplateAnno extends PadAnnotationType
object NoPadAnno extends PadAnnotationType

case class PadAnnotation(
    target: Named,
    tpe: PadAnnotationType, 
    txt: String) {
  def targetName(): String = target.name
}

// All annotations associated with IO pads transform (for module + IO signals)
object HasIOPadsAnnotation {
  def genSideAnno(side: PadSide): String = {
    val sideAnno = side match {
      case Left => "Left"
      case Right => "Right"
      case Top => "Top"
      case Bottom => "Bottom"
      case _ => throw new Exception ("Invalid IO pad annotation")
    }
    s"padSide:$sideAnno"
  }
  def noPadAnno(): String = s"noPad:##"
  def genPadNameAnno(name: String): String = s"padName:$name"
  def genTemplateFileAnno(padTemplateFile: String): String = s"padTemplateFile:$padTemplateFile"

  def apply(target: ModuleName, side: PadSide): Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], genSideAnno(side))
  def apply(target: ModuleName, padTemplateFile: String): Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], genTemplateFileAnno(padTemplateFile))
  def apply(target: ComponentName, side: PadSide): Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], genSideAnno(side))
  def apply(target: ComponentName, name: String): Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], genPadNameAnno(name))

  def noPadApply(target: ComponentName): Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], noPadAnno)

  def getSideFromAnno(a: String): PadSide = a match {
    case "Left" => Left
    case "Right" => Right
    case "Top" => Top
    case "Bottom" => Bottom
    case _ => throw new Exception("Not a valid pad side annotation!")
  }

  def unapply(a: Annotation): Option[PadAnnotation] = a match {
    case Annotation(m, t, s) if t == classOf[AddIOPadsTransform] => 
      val split = s.split(":")
      require(split.length > 0 && split.length <= 2, "Only one annotation at a time!")
      val anno = split.head match {
        case "padSide" => PadSideAnno
        case "padName" => PadNameAnno
        case "padTemplateFile" => PadTemplateAnno
        case "noPad" => NoPadAnno
        case _ => throw new Exception("Illegal pad annotation!")
      }
      // If the annotation is a 0-length string, keep the 0-length annotation
      Some(PadAnnotation(m, anno, if (split.length == 1) "" else split.last))
    case _ => None
  }
}