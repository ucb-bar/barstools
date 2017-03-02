package barstools.tapeout.transforms.clkgen

import net.jcazevedo.moultingyaml._
import firrtl.annotations._
import chisel3.experimental._
import chisel3._
import firrtl._
import firrtl.transforms.DedupModules

object ClkAnnotationsYaml extends DefaultYamlProtocol {
  implicit val _extclk = yamlFormat3(ExtClk)
  implicit val _sink = yamlFormat2(Sink)
  implicit val _clkport = yamlFormat2(ClkPortAnnotation)
  implicit val _genclk = yamlFormat3(GeneratedClk)
  implicit val _clkmod = yamlFormat2(ClkModAnnotation)
}
case class ExtClk(period: Double, waveform: Seq[Double] = Seq(), async: Seq[String] = Seq()) {
  def getWaveform = if (waveform == Seq.empty) Seq(0, period/2) else waveform
  // async = ids of top level clocks that are async with this clk
  // Default is 50% duty cycle, period units is default
  require(getWaveform.sorted == getWaveform, "Waveform edges must be in order")
  require(getWaveform.length == 2, "Must specify time for rising edge, then time for falling edge")
}

case class Sink(src: Option[ExtClk] = None , info: String = "")

case class ClkPortAnnotation(id: String, tag: Option[Sink] = None) {
  import ClkAnnotationsYaml._
  def serialize: String = this.toYaml.prettyPrint
}

abstract class ClkModType
case object ClkMux extends ClkModType {
  def serialize: String = "mux"
}
case object ClkDiv extends ClkModType {
  def serialize: String = "div"
}

// Unlike typical SDC, starts at 0. 
// Otherwise, see pg. 63 of "Constraining Designs for Synthesis and Timing Analysis" 
// by S. Gangadharan
// original clk:     |-----|_____|-----|_____|
// edges:            0     1     2     3     4
// div. by 4, 50% duty cycle --> edges = 0, 2, 4
// --->              |-----------|___________|
// sources = source id's
case class GeneratedClk(id: String, sources: Seq[String], referenceEdges: Seq[Int] = Seq()) {
  require(referenceEdges.sorted == referenceEdges, "Edges must be in order for generated clk")
  if (referenceEdges.nonEmpty) require(referenceEdges.length % 2 == 1, "# of reference edges must be odd!")
}

case class ClkModAnnotation(tpe: String, generatedClks: Seq[GeneratedClk]) {

  def modType: ClkModType = HasClkAnnotation.modType(tpe)

  modType match {
    case ClkDiv => 
      generatedClks foreach { c =>
        require(c.referenceEdges.nonEmpty, "Reference edges must be defined for clk divider!")
        require(c.sources.length == 1, "Clk divider output can only have 1 source")
      }
    case ClkMux => 
      generatedClks foreach { c =>
        require(c.referenceEdges.isEmpty, "Reference edges must not be defined for clk mux!")
      }
  }
  import ClkAnnotationsYaml._
  def serialize: String = this.toYaml.prettyPrint
}

abstract class FirrtlClkTransformAnnotation {
  def targetName: String
}

// Firrtl version
case class TargetClkModAnnoF(target: ModuleName, anno: ClkModAnnotation) extends FirrtlClkTransformAnnotation {
  def getAnno = Annotation(target, classOf[ClkSrcTransform], anno.serialize)
  def targetName = target.name
}

// Chisel version
case class TargetClkModAnnoC(target: Module, anno: ClkModAnnotation) {
  def getAnno = ChiselAnnotation(target, classOf[ClkSrcTransform], anno.serialize)
}

// Firrtl version
case class TargetClkPortAnnoF(target: ComponentName, anno: ClkPortAnnotation) extends FirrtlClkTransformAnnotation {
  def getAnno = Annotation(target, classOf[ClkSrcTransform], anno.serialize)
  def targetName = Seq(target.module.name, target.name).mkString(".")
}

// Chisel version
case class TargetClkPortAnnoC(target: Element, anno: ClkPortAnnotation) {
  def getAnno = ChiselAnnotation(target, classOf[ClkSrcTransform], anno.serialize)
}

object HasClkAnnotation {

  import ClkAnnotationsYaml._

  def modType(tpe: String): ClkModType = tpe match {
    case s: String if s == ClkMux.serialize => ClkMux
    case s: String if s == ClkDiv.serialize => ClkDiv
    case _ => throw new Exception("Clock module annotaiton type invalid")
  }

  def unapply(a: Annotation): Option[FirrtlClkTransformAnnotation] = a match {
    case Annotation(f, t, s) if t == classOf[ClkSrcTransform] => f match {
      case m: ModuleName => 
        Some(TargetClkModAnnoF(m, s.parseYaml.convertTo[ClkModAnnotation]))
      case c: ComponentName =>
        Some(TargetClkPortAnnoF(c, s.parseYaml.convertTo[ClkPortAnnotation]))
      case _ => throw new Exception("Clk source annotation only valid on module or component!")    
    }
    case _ => None
  }

  def apply(annos: Seq[Annotation]): Option[(Seq[TargetClkModAnnoF],Seq[TargetClkPortAnnoF])] = {
    // Get all clk-related annotations
    val clkAnnos = annos.map(x => unapply(x)).flatten 
    val targets = clkAnnos.map(x => x.targetName)
    require(targets.distinct.length == targets.length, "Only 1 clk related annotation is allowed per component/module")
    if (clkAnnos.length == 0) None
    else {
      val componentAnnos = clkAnnos.filter { 
        case TargetClkPortAnnoF(ComponentName(_, ModuleName(_, _)), _) => true
        case _ => false
      }.map(x => x.asInstanceOf[TargetClkPortAnnoF])
      val associatedMods = componentAnnos.map(x => x.target.module.name)
      val moduleAnnos = clkAnnos.filter { 
        case TargetClkModAnnoF(ModuleName(m, _), _) => 
          require(associatedMods contains m, "Clk modules should always have clk port annotations!")
          true 
        case _ => false
      }.map(x => x.asInstanceOf[TargetClkModAnnoF])
      Some((moduleAnnos, componentAnnos))
    }
  }

}

// Applies to both black box + normal module
trait IsClkModule {
  self: chisel3.Module =>

  def doNotDedup(module: Module): Unit = {
    annotate(ChiselAnnotation(module, classOf[DedupModules], "nodedup!"))
  }
  doNotDedup(this)

  def annotateDerivedClks(anno: ClkModAnnotation): Unit = annotateDerivedClks(this, anno)
  def annotateDerivedClks(m: Module, anno: ClkModAnnotation): Unit = 
    annotate(TargetClkModAnnoC(m, anno).getAnno)
  def annotateClkPort(p: Element, anno: ClkPortAnnotation): Unit = 
    annotate(TargetClkPortAnnoC(p, anno).getAnno)
}