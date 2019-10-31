package aoplib.breakpoint

import java.io.File

import aoplib.AnnotationHelpers
import chisel3._
import chisel3.aop.Aspect
import chisel3.aop.injecting.{InjectingAspect, InjectingTransform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.Mappers._
import _root_.firrtl.annotations.{Annotation, ModuleTarget, ReferenceTarget}
import firrtl.ir._
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, HighForm, LowForm, MidForm, PrimOps, RenameMap, ResolveAndCheck, ResolvedAnnotationPaths, Transform, WRef, WSubField, WSubIndex}

import scala.collection.mutable
import scala.io.Source
import scala.reflect.runtime.universe.TypeTag

case class BreakpointAspect[T <: RawModule, M <: RawModule](selectInstances: T => Iterable[M],
                                                            setBreakCondition: M => Bool,
                                                            selectSignals: M => Seq[Data],
                                                            selectClockReset: M => (Clock, Reset),
                                                            srcPath: String
                                                           )
                                                           (implicit tTag: TypeTag[T]) extends Aspect[T] {
  def align(strings: Seq[String]): Seq[String] = {
    strings.zip(getSpaces(strings.map(_.length))).map {
      case (str, spaces) => str + spaces
    }
  }

  def getSpaces(lengths: Seq[Int]): Seq[String] = {
    val maxLength = lengths.foldLeft(0){ (maxLength, len) => len.max(maxLength) }
    lengths.map { len => " " * (maxLength - len) }
  }

  def nDigits(width: Int): Int = {
    Math.ceil(Math.log10(Math.pow(2, width) + 1)).toInt
  }

  override def toAnnotation(top: T): AnnotationSeq = {
    InjectingAspect[T, M](
      selectInstances,
      { m: M =>
        val breakWhen = setBreakCondition(m)
        dontTouch(breakWhen)
        val nextCycle = RegInit(false.B)
        nextCycle := breakWhen
        when(nextCycle) {
          stop()
        }
        val signals = selectSignals(m).map(_.toTarget)
        val (clock, reset) = selectClockReset(m)
        annotate(new ChiselAnnotation {
          /** Conversion to FIRRTL Annotation */
          override def toFirrtl: Annotation = {
            val path = new File(".").getAbsolutePath()
            val x = Breakpoint(breakWhen.toTarget, clock.toTarget, reset.toTarget, signals, path + "/" + srcPath)
            x
          }
        })
      }
    ).toAnnotation(top) ++ Seq(RunFirrtlTransformAnnotation(new InjectingTransform), RunFirrtlTransformAnnotation(new BreakpointTransform))
  }
}

case class Breakpoint(whenBreak: ReferenceTarget, clock: ReferenceTarget, reset: ReferenceTarget, signals: Seq[ReferenceTarget], file: String) extends Annotation {
  def module = whenBreak.module
  def circuit = whenBreak.module
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newSignals = AnnotationHelpers.renameMany(signals, renames)
    val newWhenBreak = AnnotationHelpers.renameOne(whenBreak, renames)
    Seq(Breakpoint(newWhenBreak, clock, reset, newSignals, file))
  }
}


class BreakpointTransform extends Transform with ResolvedAnnotationPaths {
  override val annotationClasses: Traversable[Class[_]] = Seq(classOf[Breakpoint])
  // Import utility functions from companion object
  import Breakpoint._
  override def inputForm: CircuitForm = MidForm
  override def outputForm: CircuitForm = HighForm

  override def execute(state: CircuitState): CircuitState = {
    val moduleToBP = state.annotations.collect { case b: Breakpoint => b }.groupBy(b => b.module)

    val newMods = state.circuit.modules.map { m =>
      moduleToBP.get(m.name) match {
        case None => m
        case Some(breaks) =>
          breaks.foldLeft(m){ (mod, break) =>
            assert(break.getTargets.forall(_.asInstanceOf[ReferenceTarget].component == Nil))
            assert(break.getTargets.map(_.asInstanceOf[ReferenceTarget]).forall(_.module == break.whenBreak.module), this.toString)

            breakModule(break.whenBreak.ref, break.signals.map(_.ref), break.clock.ref, break.reset.ref, break.file)(m)
          }
      }
    }

    val newState = state.copy(circuit = state.circuit.copy(modules = newMods))
    new ResolveAndCheck().execute(newState)
  }


  def breakModule(when: String, signals: Seq[String], clock: String, reset: String, path: String)(m: DefModule): DefModule = {
    val lines = Source.fromFile(path).getLines
    val filename = getFileName(path)
    val connects = firrtl.passes.memlib.AnalysisUtils.getConnects(m)

    // Maps line number to (declaration, column number)
    val infoMap = mutable.HashMap[Int, mutable.ArrayBuffer[(IsDeclaration, Int)]]()

    // Populates infoMap
    def updateInfoMap(dec: IsDeclaration)(info: Info): Unit = {
      info match {
        case FileInfo(StringLit(i)) => i match {
          case infoRegex(file, lineNumber, columnNumber) =>
            val value = infoMap.getOrElseUpdate(lineNumber.toInt, mutable.ArrayBuffer.empty[(IsDeclaration, Int)])
            value += ((dec, columnNumber.toInt))
        }
        case MultiInfo(infos) => infos.foreach(updateInfoMap(dec))
        case NoInfo =>
      }
    }

    val signalsToRecord = signals.flatMap { s => findDeps(connects)(s) } ++ signals

    // Walks statements to populate infoMap
    def findInfo(s: Statement): Unit = {
      s match {
        case i: IsDeclaration if signalsToRecord.contains(i.name) => updateInfoMap(i)(i.info)
        case other =>
      }
      s foreachStmt findInfo
    }
    m.ports.foreach(p => updateInfoMap(p)(p.info))
    m foreachStmt findInfo

    // Now infoMap is filled in

    val annotatedScala = mutable.ArrayBuffer[Print]()
    val notReset = DoPrim(PrimOps.Not,Seq(WRef(reset)), Nil, UnknownType)
    lines.zipWithIndex.foreach { case (line, prevIndex) =>
      val index = prevIndex + 1
      if(infoMap.contains(index)) {
        val indent = selectIndent(line)
        val decs = infoMap(index).filter{ case (dec, _) => getFileName(dec.info) == filename}
        if(decs.size == 1) {
          val (dec, _) = decs.head
          // TODO: Will not work with instances or memories, WRef(dec.name)
          annotatedScala += Print(NoInfo, StringLit("%c[1;31m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
          annotatedScala += Print(NoInfo, StringLit(line), Nil, WRef(clock), notReset)
          annotatedScala += Print(NoInfo, StringLit("%c[0m"),Seq(UIntLiteral(27)), WRef(clock), notReset)

          annotatedScala += Print(NoInfo, StringLit("%c[1;34m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
          annotatedScala += Print(NoInfo, StringLit(dec.name + ":%d\n"), Seq(WRef(dec.name)), WRef(clock), notReset)
          annotatedScala += Print(NoInfo, StringLit("%c[0m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
        } else {
          annotatedScala += Print(NoInfo, StringLit("%c[1;31m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
          annotatedScala += Print(NoInfo, StringLit(line + "\n"), Nil, WRef(clock), notReset)
          annotatedScala += Print(NoInfo, StringLit("%c[0m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
          if(decs.size == 0) {
            println("HERE")
          }
          decs.foreach { case (dec, column) =>
            annotatedScala += Print(NoInfo, StringLit("%c[1;34m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
            annotatedScala += Print(
              NoInfo,
              StringLit((" " * 0.max(column - dec.name.length)) + dec.name + ":%d\n"),
              Seq(WRef(dec.name)), // TODO: Will not work with instances or memories
              WRef(clock),
              notReset
            )
            annotatedScala += Print(NoInfo, StringLit("%c[0m"),Seq(UIntLiteral(27)), WRef(clock), notReset)
          }

        }
      } else {
        annotatedScala += Print(NoInfo, StringLit(line + "\n"), Nil, WRef(clock), notReset)
      }


    }

    m match {
      case m: firrtl.ir.Module =>
        m.copy(body = Block(Seq(m.body, Conditionally(NoInfo, WRef(when), Block(annotatedScala), EmptyStmt))))
      case other => other
    }
  }

}

object Breakpoint {
  /** Matches a Firrtl info, e.g. @[ALU.scala 40:19] */
  val infoRegex = """([^ ]+) ([0-9]+):([0-9]+)""".r
  def getFileName(path: String): String = path.split("/").last
  def getFileName(info: Info): String = {
    info match {
      case FileInfo(StringLit(i)) => i match {
        case infoRegex(file, line, co) => file
        case other => ""
      }
      case other => ""
    }
  }

  def selectIndent(line: String): String = {
    val (spacing, _) = line.foldLeft(("", true)){ case ((soFar, collecting), c) =>
      if(collecting) {
        c match {
          case ' ' => (soFar + c, collecting)
          case '\t' => (soFar + c, collecting)
          case other => (soFar, false)
        }
      } else (soFar, false)
    }
    spacing
  }

  // Finds all signals who eventually drive name
  def findDeps(connects: firrtl.passes.memlib.AnalysisUtils.Connects)(name: String): collection.Set[String] = {
    val deps = mutable.HashSet[String]()
    def getDeps(e: Expression): Expression = {
      e match {
        case WRef(name, _, _, _) => deps += name
        case x: WSubField => deps += x.serialize
        case x: WSubIndex => deps += x.serialize
        case other => other map getDeps
      }
      e
    }

    connects.get(name) match {
      case None => Set.empty[String]
      case Some(e) => getDeps(e)
    }
    deps ++ deps.flatMap {
      case d if !deps.contains(d) => findDeps(connects)(d)
      case d => Set(d)
    }
  }

}
