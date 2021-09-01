// SPDX-License-Identifier: Apache-2.0

package barstools.macros

import barstools.macros.Utils.{filterForSRAM, findSRAMCompiler, readConfFromPath}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.{CircuitState, DependencyAPIMigration, Emitter, Transform}

class MacroCompilerTransform extends Transform with DependencyAPIMigration {
  override def prerequisites:          Seq[TransformDependency] = Forms.LowForm
  override def optionalPrerequisites:  Seq[TransformDependency] = Forms.LowFormOptimized
  override def optionalPrerequisiteOf: Seq[Dependency[Emitter]] = Forms.LowEmitters
  override def invalidates(a: Transform) = false

  def execute(state: CircuitState): CircuitState = {
    val macroCompilerAnnotations = state.annotations.collect { case a: MacroCompilerAnnotation => a }
    macroCompilerAnnotations match {
      case Seq(anno: MacroCompilerAnnotation) =>
        val MacroCompilerAnnotation.Params(
          memFile,
          memFileFormat,
          libFile,
          hammerIR,
          costMetric,
          mode,
          useCompiler,
          forceCompile,
          forceSynflops,
          synflopThreshold
        ) = anno.params

        if (mode == MacroCompilerAnnotation.FallbackSynflops) {
          throw new UnsupportedOperationException("Not implemented yet")
        }

        // Check that we don't have any modules both forced to compile and synflops.
        assert(forceCompile.intersect(forceSynflops).isEmpty, "Cannot have modules both forced to compile and synflops")

        // Read, eliminate None, get only SRAM, make firrtl macro
        val mems: Option[Seq[Macro]] = (memFileFormat match {
          case Some("conf") => readConfFromPath(Some(memFile))
          case _            => mdf.macrolib.Utils.readMDFFromPath(Some(memFile))
        }) match {
          case Some(x: Seq[mdf.macrolib.Macro]) =>
            Some(filterForSRAM(Some(x)).getOrElse(List()).map { new Macro(_) })
          case _ => None
        }
        val (smallMems, bigMems) = mems.get.partition { mem =>
          mem.src.width * mem.src.depth < synflopThreshold
        }
        val libs: Option[Seq[Macro]] = mdf.macrolib.Utils.readMDFFromPath(libFile) match {
          case Some(x: Seq[mdf.macrolib.Macro]) =>
            Some(filterForSRAM(Some(x)).getOrElse(List()).map { new Macro(_) })
          case _ => None
        }
        val compilers: Option[mdf.macrolib.SRAMCompiler] = mdf.macrolib.Utils.readMDFFromPath(libFile) match {
          case Some(x: Seq[mdf.macrolib.Macro]) =>
            if (useCompiler) {
              findSRAMCompiler(Some(x))
            } else None
          case _ => None
        }

        // Helper function to turn a set of mem names into a Seq[Macro].
        def setToSeqMacro(names: Set[String]): Seq[Macro] = {
          names.toSeq.map(memName => bigMems.collectFirst { case m if m.src.name == memName => m }.get)
        }

        // Build lists of memories for compilation and synflops.
        val memCompile = Some(bigMems).map { actualMems =>
          val memsAdjustedForMode = if (mode == MacroCompilerAnnotation.Synflops) Seq.empty else actualMems
          memsAdjustedForMode.filterNot(m => forceSynflops.contains(m.src.name)) ++ setToSeqMacro(forceCompile)
        }
        val memSynflops: Seq[Macro] = {
          val memsAdjustedForMode: Seq[Macro] = if (mode == MacroCompilerAnnotation.Synflops) {
            mems.getOrElse(Seq.empty)
          } else {
            smallMems
          }
          memsAdjustedForMode.filterNot(m => forceCompile.contains(m.src.name)) ++ setToSeqMacro(forceSynflops)
        }

        val transforms = Seq(
          new MacroCompilerPass(memCompile, libs, compilers, hammerIR, costMetric, mode),
          new SynFlopsPass(
            true,
            memSynflops ++ (if (mode == MacroCompilerAnnotation.CompileAndSynflops) {
                              libs.get
                            } else {
                              Seq.empty
                            })
          )
        )
        transforms.foldLeft(state)((s, xform) => xform.runTransform(s))
      case _ => state
    }
  }
}
