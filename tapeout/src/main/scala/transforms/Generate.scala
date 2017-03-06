package barstools.tapeout.transforms

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.passes.Pass

import java.io.File
import firrtl.annotations.AnnotationYamlProtocol._
import net.jcazevedo.moultingyaml._
import com.typesafe.scalalogging.LazyLogging

object AllModules {
  private var modules = Set[String]()
  def add(module: String) = {
    modules = modules | Set(module)
  }
  def rename(module: String) = {
    var new_name = module
    while (modules.contains(new_name))
      new_name = new_name + "_inTestHarness"
    new_name
  }
}

case class ParsedInput(args: Seq[String]) extends LazyLogging {
  var input: Option[String] = None
  var output: Option[String] = None
  var topOutput: Option[String] = None
  var harnessOutput: Option[String] = None
  var annoFile: Option[String] = None
  var synTop: Option[String] = None
  var harnessTop: Option[String] = None
  var seqMemFlags: Option[String] = Some("-o:unused.confg")
  var listClocks: Option[String] = Some("-o:unused.clocks")

  var usedOptions = Set.empty[Integer]
  args.zipWithIndex.foreach{ case (arg, i) =>
    arg match {
      case "-i" => {
        input = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "-o" => {
        output = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--top-o" => {
        topOutput = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--harness-o" => {
        harnessOutput = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--anno-file" => {
        annoFile = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--syn-top" => {
        synTop = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--harness-top" => {
        harnessTop = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--seq-mem-flags" => {
        seqMemFlags = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case "--list-clocks" => {
        listClocks = Some(args(i+1))
        usedOptions = usedOptions | Set(i+1)
      }
      case _ => {
        if (! (usedOptions contains i)) {
          logger.error("Unknown option " + arg)
        }
      }
    }
  }

}

trait GenerateTopAndHarnessApp extends App with LazyLogging {

  lazy val options: ParsedInput = ParsedInput(args)
  lazy val input = options.input
  lazy val output = options.output
  lazy val topOutput = options.topOutput
  lazy val harnessOutput = options.harnessOutput
  lazy val annoFile = options.annoFile
  lazy val synTop = options.synTop
  lazy val harnessTop = options.harnessTop
  lazy val seqMemFlags = options.seqMemFlags
  lazy val listClocks = options.listClocks

  private def _getTopPasses(top: Boolean, harness: Boolean): Seq[Transform] = {

    val pre = Seq(
      new ReParentCircuit(synTop.get),
      new RemoveUnusedModules
    )

    val enumerate = if (harness) { Seq(
      new EnumerateModules( { m => if (m.name != options.synTop.get) { AllModules.add(m.name) } } )
    ) } else Seq()

    val post = if (top) { Seq(
      new passes.memlib.InferReadWrite(),
      new passes.memlib.ReplSeqMem(),
      new passes.clocklist.ClockListTransform()
    ) } else Seq()

    pre ++ enumerate ++ post
  }

  private def _getHarnessPasses(top: Boolean, harness: Boolean): Seq[Transform] = {

    // always the same for now
    Seq(
      new ConvertToExtMod((m) => m.name == synTop.get),
      new RemoveUnusedModules,
      new RenameModulesAndInstances((m) => AllModules.rename(m))
    )
  }

  private def _getTopAnnotations(top: Boolean, harness: Boolean): AnnotationMap = {

    if (top) { 
      //Load annotations from file
      val annotationArray = annoFile match {
        case None => Array[Annotation]()
        case Some(fileName) => {
          val annotations = new File(fileName)
          if(annotations.exists) {
            val annotationsYaml = io.Source.fromFile(annotations).getLines().mkString("\n").parseYaml
            annotationsYaml.convertTo[Array[Annotation]]
          } else {
            Array[Annotation]()
          }
        }
      }

      // add new annotations
      AnnotationMap(Seq(
        passes.memlib.InferReadWriteAnnotation(
          s"${synTop.get}"
        ),
        passes.clocklist.ClockListAnnotation(
          s"-c:${synTop.get}:-m:${synTop.get}:${listClocks.get}"
        ),
        passes.memlib.ReplSeqMemAnnotation(
          s"-c:${synTop.get}:${seqMemFlags.get}"
        )
      ) ++ annotationArray)
    } else { AnnotationMap(Seq.empty) }
  }

  // always the same for now
  private def _getHarnessAnnotations(top: Boolean, harness: Boolean): AnnotationMap = AnnotationMap(Seq.empty)

  // Top Generation
  private def _generateTop(top: Boolean, harness: Boolean): Unit = {
    require(top || harness, "Must specify either top or harness")
    firrtl.Driver.compile(
      input.get,
      topOutput.getOrElse(output.get),
      new VerilogCompiler(),
      Parser.UseInfo,
      _getTopPasses(top, harness),
      _getTopAnnotations(top, harness)
    )

  }

  // Harness Generation
  private def _generateHarness(top: Boolean, harness: Boolean): Unit = {
    require(top || harness, "Must specify either top or harness")
    if (harness) {
      firrtl.Driver.compile(
        input.get,
        harnessOutput.getOrElse(output.get),
        new VerilogCompiler(),
        Parser.UseInfo,
        _getHarnessPasses(top, harness),
        _getHarnessAnnotations(top, harness)
      )
    }
    // else do nothing, since there's no need to generate a harness when just generating a top
  }

  // generates a verilog test harness
  def generateHarness: Unit = {

    // warn about unused options
    topOutput match {
      case Some(value) => logger.warn("Not using top-level output filename $value since you asked for just a test harness.")
      case None => 
    }
    annoFile match {
      case Some(value) => logger.warn("Not using annotations file $value since you asked for just a test harness.")
      case None => 
    }
    seqMemFlags match {
      case Some("-o:unused.confg") => 
      case Some(value) => logger.warn("Not using SeqMem flags $value since you asked for just a test harness.")
      case None => 
    }
    listClocks match {
      case Some("-o:unused.clocks") => 
      case Some(value) => logger.warn("Not using clocks list $value since you asked for just a test harness.")
      case None => 
    }
    harnessOutput match {
      case Some(value) => output match {
        case Some(value2) => logger.warn("Not using generic output filename $value2 since you asked for just a test harness and also specified a generic output.") 
        case None => 
      }
      case None => 
    }

    // generate test harness
    _generateTop(top = false, harness = true)
    _generateHarness(top = false, harness = true)
  }

  // generates a top-level verilog
  def generateTop: Unit = {

    // warn about unused options
    harnessOutput match {
      case Some(value) => logger.warn("Not using harness output filename $value since you asked for just a top-level output.")
      case None => 
    }
    topOutput match {
      case Some(value) => output match {
        case Some(value2) => logger.warn("Not using generic output filename $value2 since you asked for just a top-level output and also specified a generic output.") 
        case None =>
      }
      case None => 
    }

    // generate top
    _generateTop(top = true, harness = false)
    _generateHarness(top = true, harness = false) // does nothing, but here for symmetry!
  }

  // generates both a top-level verilog and verilog test harness
  def generateTopAndHarness: Unit = {

    // warn about unused options
    output match {
      case Some(value) => logger.warn("Not using generic output filename $value since you asked for both a top-level output and a test harness.")
      case None => 
    }

    // generate both
    _generateTop(top = true, harness = true)
    _generateHarness(top = true, harness = true)
  }

}

object GenerateTop extends GenerateTopAndHarnessApp {
  generateTop
}

object GenerateHarness extends GenerateTopAndHarnessApp {
  generateHarness
}

object GenerateTopAndHarness extends GenerateTopAndHarnessApp {
  generateTopAndHarness
}

