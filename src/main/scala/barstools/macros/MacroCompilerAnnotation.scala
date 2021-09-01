// SPDX-License-Identifier: Apache-2.0

package barstools.macros

import firrtl.annotations.NoTargetAnnotation

// TODO The parameters could be unpacked here instead of keeping it in a serialized form
case class MacroCompilerAnnotation(content: String) extends NoTargetAnnotation {
  import MacroCompilerAnnotation.Params

  def params: Params = MacroCompilerUtil.objFromString(content).asInstanceOf[Params]
}

/** The MacroCompilerAnnotation to trigger the macro compiler.
  * Note that this annotation does NOT actually target any modules for
  * compilation. It simply holds all the settings for the memory compiler. The
  * actual selection of which memories to compile is set in the Params.
  *
  * To use, simply annotate the entire circuit itself with this annotation and
  * include [[MacroCompilerTransform]].
  */
object MacroCompilerAnnotation {

  /** Macro compiler mode. */
  sealed trait CompilerMode

  /** Strict mode - must compile all memories or error out. */
  case object Strict extends CompilerMode

  /** Synflops mode - compile all memories with synflops (do not map to lib at all). */
  case object Synflops extends CompilerMode

  /** Do not compile memories that are below threshold size (width * depth) */
  case class SynflopThreshold(threshold: Int) extends CompilerMode

  /** CompileAndSynflops mode - compile all memories and create mock versions of the target libs with synflops. */
  case object CompileAndSynflops extends CompilerMode

  /** FallbackSynflops - compile all memories to SRAM when possible and fall back to synflops if a memory fails. * */
  case object FallbackSynflops extends CompilerMode

  /** CompileAvailable - compile what is possible and do nothing with uncompiled memories. * */
  case object CompileAvailable extends CompilerMode

  /** The default mode for the macro compiler.
    * TODO: Maybe set the default to FallbackSynflops (typical for
    * vlsi_mem_gen-like scripts) once it's implemented?
    */
  val Default: CompilerMode = CompileAvailable

  // Options as list of (CompilerMode, command-line name, description)
  val options: Seq[(CompilerMode, String, String)] = Seq(
    (Default, "default", "Select the default option from below."),
    (Strict, "strict", "Compile all memories to library or return an error."),
    (
      Synflops,
      "synflops",
      "Produces synthesizable flop-based memories for all memories (do not map to lib at all); likely useful for simulation purposes."
    ),
    (
      CompileAndSynflops,
      "compileandsynflops",
      "Compile all memories and create mock versions of the target libs with synflops; likely also useful for simulation purposes."
    ),
    (
      FallbackSynflops,
      "fallbacksynflops",
      "Compile all memories to library when possible and fall back to synthesizable flop-based memories when library synth is not possible."
    ),
    (
      CompileAvailable,
      "compileavailable",
      "Compile all memories to library when possible and do nothing in case of errors. (default)"
    )
  )

  /** Helper function to select a compiler mode. */
  def stringToCompilerMode(str: String): CompilerMode = options.collectFirst {
    case (mode, cmd, _) if cmd == str => mode
  } match {
    case Some(x) => x
    case None    => throw new IllegalArgumentException("No such compiler mode " + str)
  }

  /** Parameters associated to this MacroCompilerAnnotation.
    *
    * @param mem               Path to memory lib
    * @param memFormat         Type of memory lib (Some("conf"), Some("mdf"), or None (defaults to mdf))
    * @param lib               Path to library lib or None if no libraries
    * @param hammerIR          Path to HammerIR output or None (not generated in this case)
    * @param costMetric        Cost metric to use
    * @param mode              Compiler mode (see CompilerMode)
    * @param forceCompile      Set of memories to force compiling to lib regardless of the mode
    * @param forceSynflops     Set of memories to force compiling as flops regardless of the mode
    * @param synflopThreshold Memories below this side are forced to synflops
    */
  case class Params(
    mem:              String,
    memFormat:        Option[String],
    lib:              Option[String],
    hammerIR:         Option[String],
    costMetric:       CostMetric,
    mode:             CompilerMode,
    useCompiler:      Boolean,
    forceCompile:     Set[String],
    forceSynflops:    Set[String],
    synflopThreshold: Long)
      extends Serializable

  /** Create a MacroCompilerAnnotation.
    * @param c Top-level circuit name (see class description)
    * @param p Parameters (see above).
    */
  def apply(c: String, p: Params): MacroCompilerAnnotation =
    MacroCompilerAnnotation(MacroCompilerUtil.objToString(p))

}
