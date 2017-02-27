package barstools.tapeout.transforms.pads

import firrtl._
import firrtl.annotations._
import firrtl.passes._
import firrtl.ir._

// Main Add IO Pad transform operates on low Firrtl
class AddIOPadsTransform extends Transform with SimpleRun {

  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  override def execute(state: CircuitState): CircuitState = {
    val collectedAnnos = HasPadAnnotation(getMyAnnotations(state))
    collectedAnnos match {
      // Transform not used
      case None => CircuitState(state.circuit, LowForm)
      case Some(x) => 
        // Get foundry pad templates from yaml
        val foundryPads = FoundryPadsYaml.parse(x.padTemplateFile)
        val portPads = AnnotatePortPads(state.circuit, x.topModName, foundryPads, x.componentAnnos, 
          HasPadAnnotation.getSide(x.defaultPadSide))
        val supplyPads = AnnotateSupplyPads(foundryPads, x.supplyAnnos)
        val (circuitWithBBs, bbAnnotations) = CreatePadBBs(state.circuit, portPads, supplyPads)
        val passSeq = Seq(
          Legalize,
          ResolveGenders,
          // Types really need to be known...
          InferTypes,
          new AddPadFrame(x.topModName, portPads, supplyPads),
          RemoveEmpty,
          CheckInitialization,
          InferTypes,
          Uniquify,
          ResolveKinds,
          ResolveGenders
        )
        // Expects BlackBox helper to be run after to inline pad Verilog!
        val prevAnnos = state.annotations.getOrElse(AnnotationMap(Seq.empty)).annotations
        val cs = CircuitState(
          runPasses(circuitWithBBs, passSeq), 
          LowForm, 
          Some(AnnotationMap(prevAnnos ++ bbAnnotations))
        )
        (new firrtl.transforms.BlackBoxSourceHelper).execute(cs)
    }
  }
}