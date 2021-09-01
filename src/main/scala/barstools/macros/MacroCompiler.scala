// SPDX-License-Identifier: BSD-3-Clause

/** Terminology note:
  * mem - target memory to compile, in design (e.g. Mem() in rocket)
  * lib - technology SRAM(s) to use to compile mem
  */

package barstools.macros

import barstools.macros.Utils._
import firrtl._
import firrtl.ir._
import firrtl.stage.{FirrtlSourceAnnotation, FirrtlStage, OutputFileAnnotation, RunFirrtlTransformAnnotation}
import firrtl.transforms.NoDCEAnnotation

import java.io.{File, FileWriter}
import scala.annotation.tailrec

object MacroCompiler extends App {
  sealed trait MacroParam
  case object Macros extends MacroParam
  case object MacrosFormat extends MacroParam
  case object Library extends MacroParam
  case object Verilog extends MacroParam
  case object Firrtl extends MacroParam
  case object HammerIR extends MacroParam
  case object CostFunc extends MacroParam
  case object Mode extends MacroParam
  case object UseCompiler extends MacroParam
  case object SynflopThreshold extends MacroParam

  type MacroParamMap = Map[MacroParam, String]
  type CostParamMap = Map[String, String]
  type ForcedMemories = (Set[String], Set[String])
  val modeOptions: Seq[String] = MacroCompilerAnnotation.options.map { case (_, cmd, description) =>
    s"    $cmd: $description"
  }
  val usage: String = (Seq(
    "Options:",
    "  -n, --macro-conf: The set of macros to compile in firrtl-generated conf format (exclusive with -m)",
    "  -m, --macro-mdf: The set of macros to compile in MDF JSON format (exclusive with -n)",
    "  -l, --library: The set of macros that have blackbox instances",
    "  -u, --use-compiler: Flag, whether to use the memory compiler defined in library",
    "  -v, --verilog: Verilog output",
    "  -f, --firrtl: FIRRTL output (optional)",
    "  -hir, --hammer-ir: Hammer-IR output currently only needed for IP compilers",
    "  -c, --cost-func: Cost function to use. Optional (default: \"default\")",
    "  -cp, --cost-param: Cost function parameter. (Optional depending on the cost function.). e.g. -c ExternalMetric -cp path /path/to/my/cost/script",
    "  --force-compile [mem]: Force the given memory to be compiled to target libs regardless of the mode",
    "  --force-synflops [mem]: Force the given memory to be compiled via synflops regardless of the mode",
    "  --mode:"
  ) ++ modeOptions).mkString("\n")

  @tailrec
  def parseArgs(
    map:            MacroParamMap,
    costMap:        CostParamMap,
    forcedMemories: ForcedMemories,
    args:           List[String]
  ): (MacroParamMap, CostParamMap, ForcedMemories) =
    args match {
      case Nil => (map, costMap, forcedMemories)
      case ("-n" | "--macro-conf") :: value :: tail =>
        parseArgs(map + (Macros -> value) + (MacrosFormat -> "conf"), costMap, forcedMemories, tail)
      case ("-m" | "--macro-mdf") :: value :: tail =>
        parseArgs(map + (Macros -> value) + (MacrosFormat -> "mdf"), costMap, forcedMemories, tail)
      case ("-l" | "--library") :: value :: tail =>
        parseArgs(map + (Library -> value), costMap, forcedMemories, tail)
      case ("-u" | "--use-compiler") :: tail =>
        parseArgs(map + (UseCompiler -> ""), costMap, forcedMemories, tail)
      case ("-v" | "--verilog") :: value :: tail =>
        parseArgs(map + (Verilog -> value), costMap, forcedMemories, tail)
      case ("-f" | "--firrtl") :: value :: tail =>
        parseArgs(map + (Firrtl -> value), costMap, forcedMemories, tail)
      case ("-hir" | "--hammer-ir") :: value :: tail =>
        parseArgs(map + (HammerIR -> value), costMap, forcedMemories, tail)
      case ("-c" | "--cost-func") :: value :: tail =>
        parseArgs(map + (CostFunc -> value), costMap, forcedMemories, tail)
      case ("-cp" | "--cost-param") :: value1 :: value2 :: tail =>
        parseArgs(map, costMap + (value1 -> value2), forcedMemories, tail)
      case "--force-compile" :: value :: tail =>
        parseArgs(map, costMap, forcedMemories.copy(_1 = forcedMemories._1 + value), tail)
      case "--force-synflops" :: value :: tail =>
        parseArgs(map + (Macros -> value) + (MacrosFormat -> "conf"), costMap, forcedMemories, tail)
      case "--synflop-threshold" :: value :: tail =>
        parseArgs(map + (SynflopThreshold -> value), costMap, forcedMemories, tail)
      case "--mode" :: value :: tail =>
        parseArgs(map + (Mode -> value), costMap, forcedMemories, tail)
      case "--help" :: _ =>
        println(s"MacroCompiler help\n")
        println(usage)
        sys.exit(0)
      case arg :: _ =>
        println(s"Unknown field $arg\n")
        println(usage)
        sys.exit(1)
    }

  def run(args: List[String]): Unit = {
    val (params, costParams, forcedMemories) =
      parseArgs(Map[MacroParam, String](), Map[String, String](), (Set.empty, Set.empty), args)
    try {
      val macros = params.get(MacrosFormat) match {
        case Some("conf") =>
          filterForSRAM(readConfFromPath(params.get(Macros))).get.map(x => new Macro(x).blackbox)
        case _ =>
          filterForSRAM(mdf.macrolib.Utils.readMDFFromPath(params.get(Macros))).get
            .map(x => new Macro(x).blackbox)
      }

      if (macros.nonEmpty) {
        // Note: the last macro in the input list is (seemingly arbitrarily)
        // determined as the firrtl "top-level module".
        val circuit = Circuit(NoInfo, macros, macros.last.name)
        val annotations = AnnotationSeq(
          Seq(
            MacroCompilerAnnotation(
              circuit.main,
              MacroCompilerAnnotation.Params(
                params(Macros),
                params.get(MacrosFormat),
                params.get(Library),
                params.get(HammerIR),
                CostMetric.getCostMetric(params.getOrElse(CostFunc, "default"), costParams),
                MacroCompilerAnnotation.stringToCompilerMode(params.getOrElse(Mode, "default")),
                params.contains(UseCompiler),
                forceCompile = forcedMemories._1,
                forceSynflops = forcedMemories._2,
                synflopThreshold = params.getOrElse(SynflopThreshold, "0").toLong
              )
            )
          )
        )

        // The actual MacroCompilerTransform basically just generates an input circuit
        val macroCompilerInput = CircuitState(circuit, annotations)
        val macroCompiled = (new MacroCompilerTransform).execute(macroCompilerInput)

        // Run FIRRTL compiler
        (new FirrtlStage).execute(
          Array.empty,
          Seq(
            OutputFileAnnotation(params.getOrElse(Verilog, "")),
            RunFirrtlTransformAnnotation(new VerilogEmitter),
            EmitCircuitAnnotation(classOf[VerilogEmitter]),
            NoDCEAnnotation,
            FirrtlSourceAnnotation(macroCompiled.circuit.serialize)
          )
        )

        params.get(HammerIR) match {
          case Some(hammerIRFile: String) =>
            val lines = FileUtils.getLines(hammerIRFile).toList
            val hammerIRWriter = new FileWriter(new File(hammerIRFile))
            // JSON means we need to destroy the last comma :(
            lines.dropRight(1).foreach(l => hammerIRWriter.write(l + "\n"))
            hammerIRWriter.write("]\n")
            hammerIRWriter.close()
          case None =>
        }
      } else {
        // Warn user
        System.err.println("WARNING: Empty *.mems.conf file. No memories generated.")

        // Emit empty verilog file if no macros found
        params.get(Verilog) match {
          case Some(verilogFile: String) =>
            // Create an empty verilog file
            val verilogWriter = new FileWriter(new File(verilogFile))
            verilogWriter.close()
          case None =>
        }
      }
    } catch {
      case e: java.util.NoSuchElementException =>
        if (args.isEmpty) {
          println("Command line arguments must be specified")
        } else {
          e.printStackTrace()
        }
        e.printStackTrace()
        sys.exit(1)
      case e: MacroCompilerException =>
        println(usage)
        e.printStackTrace()
        sys.exit(1)
      case e: Throwable =>
        throw e
    }
  }

  run(args.toList)
}
