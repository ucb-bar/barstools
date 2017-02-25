package barstools.tapeout.transforms.pads
import barstools.tapeout.transforms._
import firrtl.annotations._
import chisel3.experimental._

// IO Port can either be annotated with padName + padSide OR noPad (mutually exclusive)
abstract class IOAnnotation extends AnnotationSerializer
case class IOPadAnnotation(padSide: PadSide, padName: String) extends IOAnnotation
case object NoIOPadAnnotation extends IOAnnotation {
  val noPad = "##"
}
// Firrtl version
case class TargetIOPadAnnoF(target: ComponentName, anno: IOAnnotation) {
  def genFirrtlAnno: Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], anno.serialize)
}
// Chisel version
case class TargetIOPadAnnoC(target: Bits, anno: IOAnnotation) {
  def genChiselAnno: ChiselAnnotation = 
    ChiselAnnotation(target, classOf[AddIOPadsTransform], anno.serialize)
}

// A bunch of supply pads (designated by name, # on each chip side) can be associated with the top module
case class SupplyAnnotation(
    padName: String, 
    leftSide: Int = 0, 
    rightSide: Int = 0, 
    topSide: Int = 0, 
    bottomSide: Int = 0) extends AnnotationSerializer {
  override def innerSplit: String = "~"
  override def outerSplit: String = ";"
}
case class SupplyAnnos(annos: Seq[SupplyAnnotation]) extends SimpleAnnotation {
  def serialize: String = annos.map(_.serialize).mkString("+")
}
// The chip top should have a default pad side, a pad template file, and supply annotations
case class ModulePadAnnotation(
    defaultPadSide: PadSide, 
    padTemplateFile: String, 
    supplyAnnos: SupplyAnnos) extends AnnotationSerializer
// Firrtl version
case class TargetModulePadAnnoF(target: ModuleName, anno: ModulePadAnnotation) {
  def genFirrtlAnno: Annotation = 
    Annotation(target, classOf[AddIOPadsTransform], anno.serialize)  
}
// Chisel version
case class TargetModulePadAnnoC(target: Bits, anno: ModulePadAnnotation) {
  def genChiselAnno: ChiselAnnotation = 
    ChiselAnnotation(target, classOf[AddIOPadsTransform], anno.serialize)
}