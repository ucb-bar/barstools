package barstools.tapeout.transforms.pads

import firrtl._
import firrtl.annotations._
import firrtl.passes._
import firrtl.ir._

// Main Add IO Pad transform operates on low Firrtl
class AddIOPadsTransform extends Transform with SimpleRun {

  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  case class AddIOPadAnnotations(
      topMod: String,
      foundryPads: Seq[IOPad],
      componentAnnos: Seq[PadAnnotation],
      defaultSide: PadSide
  )

  def handleChiselAnnotations(state: CircuitState): Option[AddIOPadAnnotations] = {
    // Get all pad-related annotations (yaml file, side, name, etc.)
    val padAnnos = getMyAnnotations(state).map { 
      case HasIOPadsAnnotation(x) => Some(x)
      case _ => None 
    }.flatten

    // Get annotation for default pad side of top module
    val defaultModuleSideAnno = padAnnos.filter { 
      case PadAnnotation(ModuleName(_, _), PadSideAnno, _) => true 
      case _ => false
    }
    require(defaultModuleSideAnno.length <= 1, "Can only specify default pad side once!")

    // Needs to find template annotation to know that you're supposed to use the transform
    if (defaultModuleSideAnno.length == 0) None
    else {
      // Actually get module side
      val defaultModuleSide = HasIOPadsAnnotation.getSideFromAnno(defaultModuleSideAnno.head.txt)

      // Get template file associated with pads
      val templateFileAnno = padAnnos.filter { 
        case PadAnnotation(ModuleName(_, _), PadTemplateAnno, _) => true 
        case _ => false
      }
      require(templateFileAnno.length == 1, "Can only specify pad yaml file once!")
      
      // Get top module to add pads too
      val topMod = defaultModuleSideAnno.head.target.name
      require(topMod == templateFileAnno.head.target.name,
        "Module annotations for pad transform must be on the same module!")

      // Get IO (port) annotations
      val componentAnnos = padAnnos.filter { 
        case PadAnnotation(ComponentName(_, ModuleName(topMod, _)), _, _) => true 
        case _ => false
      }

      // Get foundry pad templates from yaml
      import IOPadsYamlProtocol._
      val foundryPads = (new IOPadsYamlFileReader(templateFileAnno.head.txt)).parse

      Some(AddIOPadAnnotations(topMod, foundryPads, componentAnnos, defaultModuleSide))
    }
  }

  override def execute(state: CircuitState): CircuitState = {
    val chiselAnnotations = handleChiselAnnotations(state)
    chiselAnnotations match {
      // Transform not used
      case None => CircuitState(state.circuit, LowForm)
      case Some(x) => {
        val portPads = AnnotatePortPads(state.circuit, x.topMod, x.foundryPads, x.componentAnnos, x.defaultSide)
        val (circuitWithBBs, bbAnnotations) = CreatePadBBs(state.circuit, x.topMod, portPads)

        val passSeq = Seq(
          Legalize,
          ResolveGenders,
          new AddIOPads(portPads),
          RemoveEmpty,
          CheckInitialization,
          InferTypes,
          Uniquify,
          ResolveKinds,
          ResolveGenders
        )

        // Expects BlackBox helper to be run after to inline pad Verilog!
        CircuitState(runPasses(circuitWithBBs, passSeq), LowForm, annotations = Some(AnnotationMap(bbAnnotations)))
      }
    }
  }
}