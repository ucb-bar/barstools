package barstools.tapeout.transforms.pads

import net.jcazevedo.moultingyaml._

import firrtl._
import firrtl.ir._

case class ChipPad(
    tpe: String, 
    name: String,     
    verilog: String) {

  require(verilog.contains("{{#if isHorizontal}}"), "All pad templates must contain '{{#if isHorizontal}}'")
  require(verilog.contains("{{name}}"), "All pad templates must contain module name '{{name}}'")

  val padType = tpe match {
    case "digital" => 
      require(verilogTemplate.contains("in"), "Digital pad template must contain input called 'in'")
      require(verilogTemplate.contains("out"), "Digital pad template must contain output called 'out'")
      require(verilogTemplate.contains("{{#if isInput}}"), "Digital pad template must contain '{{#if isInput}}'")
      DigitalPad
    case "analog" => 
      require(verilogTemplate.contains("io"), "Analog pad template must contain inout called 'io'")
      require(!verilogTemplate.contains("{{#if isInput}}"), "Analog pad template must not contain '{{#if isInput}}'")
      AnalogPad
    case "supply" => 
      // Supply pads don't have IO
      require(!verilogTemplate.contains("{{#if isInput}}"), "Supply pad template must not contain '{{#if isInput}}'")
      SupplyPad
    case _ => throw new Exception("Illegal pad type in config!")
  }

  import com.gilt.handlebars.scala.binding.dynamic._
  import com.gilt.handlebars.scala.Handlebars
  private val template = Handlebars(verilogTemplate)

  // Make sure names don't have spaces in Verilog!
  private[barstools] val correctedName = name.replace(" ", "_")

  case class TemplateParams(
      // isInput only used with digital pads
      isInput: Boolean,
      isHorizontal: Boolean) {

    private val orient = if (isHorizontal) Horizontal.serialize else Vertical.serialize
    private val dir = padType match {
      case AnalogPad => InOut.serialize
      case SupplyPad => NoDirection.serialize
      case DigitalPad => if (isInput) Input.serialize else Output.serialize
    }
    val name = {
      val start = Seq("pad", tpe, correctedName, orient)
      if (padType == DigitalPad) start :+ dir
      else start
    }.mkString("_")
  }

  // Note: Analog + supply don't use direction
  private def getTemplateParams(dir: Direction, orient: PadOrientation): TemplateParams = 
    TemplateParams(isInput = (dir == Input), isHorizontal = (orient == Horizontal))

  def getVerilog(dir: Direction, orient: PadOrientation): String = {
    val p = getTemplateParams(dir, orient)
    template(p)
  }

  def getName(dir: Direction, orient: PadOrientation): String = getTemplateParams(dir, orient).name
  def getArrayName(dir: Direction, orient: PadOrientation): String = 
    Seq(getName(dir, orient), "array").mkString("_")
}

object ChipPadsYaml extends DefaultYamlProtocol {
  val exampleResource "/ChipPads.yaml"
  implicit val _pad = yamlFormat3(ChipPad)
  def parse(file: String = ""): Seq[ChipPad] = {
    val out = (new YamlFileReader(exampleResource)).parse[ChipPad](file)
    val padNames = out.map(x => x.correctedName)
    require(padNames.distinct.length == padNames.length, "Pad names must be unique!")
    out
  }
}