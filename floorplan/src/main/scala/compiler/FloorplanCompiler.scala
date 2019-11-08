// See LICENSE for license details
package barstools.floorplan.compiler

import barstools.floorplan._

case class FloorplanOptions(
  inputFloorplanFile: String = "",
  outputFloorplanFile: String = ""
)

object FloorplanCompiler extends App {

  val opts = (new scopt.OptionParser[FloorplanOptions]("fpCompiler") {

    opt[String]('i', "input-file").
      required().
      valueName("<input file>").
      action((x, c) => c.copy(inputFloorplanFile = x)).
      text("input file name")

    opt[String]('o', "output-file").
      required().
      valueName("<output file>").
      action((x, c) => c.copy(outputFloorplanFile = x)).
      text("output file name")

  }).parse(args, FloorplanOptions()).getOrElse {
    throw new Exception("Error parsing options!")
  }

}
