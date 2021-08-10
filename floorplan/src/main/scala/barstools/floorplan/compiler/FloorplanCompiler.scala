// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._
import java.io.{File, FileWriter}

case class FloorplanOptions(
  outFile: String = "",
  topMod: String = "",
  outFmt: OutputFormat = OutputFormat.HammerIR,
  inFiles: Seq[String] = Seq(),
  sbAnnoFiles: Seq[String] = Seq(),
  memInstMapFiles: Seq[String] = Seq(),
  debugFile: Option[String] = None
)

object FloorplanCompiler extends App {

  val opts = (new scopt.OptionParser[FloorplanOptions]("fpCompiler") {

    opt[String]('i', "input-file").
      required().
      valueName("<input file>").
      action((x, c) => c.copy(inFiles = c.inFiles :+ x)).
      text("input file name")

    opt[String]('t', "top-module").
      required().
      valueName("<top module name>").
      action((x, c) => c.copy(topMod = x)).
      text("top module name")

    opt[String]('o', "output-file").
      required().
      valueName("<output file>").
      action((x, c) => c.copy(outFile = x)).
      text("output file name")

    opt[String]('m', "mem-inst-file").
      required().
      valueName("<mem inst file>").
      action((x, c) => c.copy(memInstMapFiles = c.memInstMapFiles :+ x)).
      text("file containing the memory instance map")

    opt[String]('b', "oob-anno-file").
      required().
      valueName("<out-of-band anno file>").
      action((x, c) => c.copy(sbAnnoFiles = c.sbAnnoFiles :+ x)).
      text("output file name")

    opt[Unit]('f', "output-fpir").
      action((x, c) => c.copy(outFmt = OutputFormat.FloorplanIR)).
      text("emit floorplanIR")

    opt[String]('d', "debug-file").
      action((x, c) => c.copy(debugFile = Some(x))).
      text("debug file path")

  }).parse(args, FloorplanOptions()).getOrElse {
    throw new Exception("Error parsing options!")
  }

  // TODO make Passes customizable
  val fpStateIn = FloorplanState.fromFiles(opts.inFiles)
  val debugWriter = opts.debugFile.map(x => new FileWriter(new File(x)))
  debugWriter.foreach(_.write("Input state:\n\n"))
  val fpStateOut = Pass.all(opts).foldLeft(fpStateIn) { (state, pass) =>
    debugWriter.foreach(_.write(FloorplanState.serialize(state)))
    debugWriter.foreach(_.write("\n\nNext state:\n\n"))
    pass.execute(state)
  }
  debugWriter.foreach(_.write(FloorplanState.serialize(fpStateOut)))
  debugWriter.foreach(_.close())
  FloorplanState.toFile(opts.outFile, opts.outFmt, fpStateOut)

}
