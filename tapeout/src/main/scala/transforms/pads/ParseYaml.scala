package barstools.tapeout.transforms.pads

import net.jcazevedo.moultingyaml._
import java.io.File

import firrtl._
import firrtl.ir._

trait PadOrientation
object Horizontal extends PadOrientation
object Vertical extends PadOrientation

case class IOPad(
    tpe: String, 
    name: String,     
    verilogTemplate: String) {

  val moduleNamePrefix = s"pad_${tpe}_${name}"

  tpe match {
    case "digital" | "analog" | "supply" => 
    case _ => throw new Exception("Illegal pad type in config!")
  }

  import com.gilt.handlebars.scala.binding.dynamic._
  import com.gilt.handlebars.scala.Handlebars
  val template = Handlebars(verilogTemplate)

  case class TemplateParams(
      isInput: Boolean,
      isHorizontal: Boolean) {
    private val dir = tpe match {
      case "analog" => "inout"
      case "supply" => "none"
      case "digital" => if (isInput) "in" else "out"
    }
    private val orient = if (isHorizontal) "H" else "V"
    // Only digital (not analog, supply) has multiple directions
    private val nameSuffix = if (tpe == "digital") s"_${dir}" else ""
    val name = s"${moduleNamePrefix}_${orient}${nameSuffix}"
  }

  def getTemplateParams(dir: Direction, orient: PadOrientation): TemplateParams = 
    TemplateParams(isInput = (dir == Input), isHorizontal = (orient == Horizontal))

  def createVerilog(dir: Direction, orient: PadOrientation): String = {
    val p = getTemplateParams(dir, orient)
    template(p)
  }
}

object IOPadsYamlProtocol extends DefaultYamlProtocol {
  implicit val _pad = yamlFormat3(IOPad)
}

// Get pads associated with tech from Yaml config file
class IOPadsYamlFileReader(file: String) {
  def parse(implicit reader: YamlReader[IOPad]) : Seq[IOPad] = {
    // If the user doesn't provide a Yaml file name, use defaults
    val yamlString = { file match {
      case f if f.isEmpty => 
        // Use example config if no file is provided
        val stream = getClass.getResourceAsStream("/IOPads.yaml")
        io.Source.fromInputStream(stream).mkString
      case f if new File(f).exists => 
        scala.io.Source.fromFile(f).getLines.mkString("\n")
      case _ => 
        throw new Exception("No valid Yaml file found!")
    }}
    val out = yamlString.parseYamls.map(x => reader.read(x))
    val padNames = out.map( x => x.name)
    require(padNames.distinct.length == padNames.length, "Pad names must be unique!")
    out
  }
}

