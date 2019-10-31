package aoplib

import chisel3.{Bool, Data}
import firrtl.annotations.{Annotation, ReferenceTarget, Target}
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, HighForm, IRToWorkingIR, LowForm, RenameMap, ResolveAndCheck, ResolvedAnnotationPaths, Transform, UNKNOWNGENDER, WRef, WSubField, WSubIndex}
import firrtl.ir._
import firrtl.Mappers._

import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag



//abstract class LoggingInfo(message: String, signals: Seq[Data], clock: chisel3.Clock, enable: Option[Bool] = None) extends AspectInfo {


//case class FirrtlLoggingInfo(enable: ReferenceTarget, signals: Seq[ReferenceTarget], clock: ReferenceTarget, message: String) {

//abstract class LoggingAspect[T <: RawModule](implicit tag: TypeTag[T]) extends Aspect[T, LoggingInfo] {
//
//  def logSignals(info: LoggingInfo): Unit
//
//  override def collectAspectInfo(dut: T): Seq[LoggingInfo] = logSignals(dut)
//
//  override def resolveAspectInfo(aspectInfo: Seq[LoggingInfo]): AnnotationSeq = {
//    val fLogSignals = aspectInfo.map(i => FirrtlLoggingInfo(i.message, i.signals.map(_.toTarget), i.clock.toTarget, i.enable.map(_.toTarget)))
//    Seq(LoggingSignals(fLogSignals))
//  }
//
//  override def transformClass: Class[_ <: Transform] = classOf[LoggingTransform]
//}


// FIRRTL STUFF BELOW

/*

case class FirrtlLoggingInfo(message: String, signals: Seq[ReferenceTarget], clock: ReferenceTarget, enable: Option[ReferenceTarget]) {
  private val circuits = getTargets.map(t => t.circuit).toSet
  private val modules = getTargets.map(t => t.module).toSet
  assert(circuits.size == 1 && modules.size == 1, s"All logging signals must share the same circuits $circuits and modules $modules: $this")
  val circuit: String = circuits.head
  val module: String = modules.head
  val enRef: Option[String] = enable.map(_.ref)
  val sigRefs: Seq[String] = signals.map(_.ref)
  val clkRef: String = clock.ref
  def getTargets: Seq[ReferenceTarget] = clock +: (signals ++ enable)
}


case class LoggingSignals(infos: Seq[FirrtlLoggingInfo]) extends Annotation with AnnotationHelpers {
  override def getTargets: Seq[ReferenceTarget] = infos.flatMap(i => i.getTargets)
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newTargets = infos.map{ i =>
      FirrtlLoggingInfo(i.message, renameMany(i.signals, renames), renameOne(i.clock, renames), i.enable.map(e => renameOne(e, renames)))
    }
    Seq(LoggingSignals(newTargets))
  }
}

class LoggingTransform extends Transform with ResolvedAnnotationPaths {
  override def inputForm: CircuitForm = HighForm
  override def outputForm: CircuitForm = HighForm


  override val annotationClasses: Traversable[Class[_]] = Seq(classOf[LoggingSignals])

  override protected def execute(state: CircuitState): CircuitState = {
    val infos = state.annotations.collect{ case a: LoggingSignals => a}.flatMap( a => a.infos )
    println("Executing Logging Transform on the following:")

    new ResolveAndCheck().runTransform(state.copy(circuit = addLogs(state.circuit, infos)))
  }

  def addLogs(circuit: Circuit, infos: Seq[FirrtlLoggingInfo]): Circuit = {
    val moduleSignalMap = mutable.HashMap[String, mutable.ArrayBuffer[FirrtlLoggingInfo]]()
    infos.foreach { i =>
      i.getTargets.foreach { t =>
        assert(/*t.component == Nil &&*/ t.path == Nil)
      }
      moduleSignalMap.getOrElseUpdate(i.module, mutable.ArrayBuffer[FirrtlLoggingInfo]()) += i
    }
    def toExp(r: ReferenceTarget): Expression = {
      def toWExp(e: Expression): Expression = e map toWExp match {
        case Reference(name, _) => WRef(name)
        case SubField(expr, name, _) => WSubField(expr, name)
        case SubIndex(expr, idx, _) => WSubIndex(expr, idx, UnknownType, UNKNOWNGENDER)
      }
      toWExp(r.toNamed.expr)
    }

    def addModuleLogs(logStuff: Seq[FirrtlLoggingInfo])(stmt: Statement): Statement = {
      val prints: Seq[Print] = logStuff.map { i =>
        val enable = if(i.enable.isDefined) toExp(i.enable.get) else UIntLiteral(1)
        val x = Print(NoInfo, StringLit(i.message), i.signals.map(toExp), toExp(i.clock), enable)
        println(x.serialize)
        x
      }
      Block(stmt +: prints)
    }

    circuit map { m: DefModule =>
      if(moduleSignalMap.contains(m.name)) m map addModuleLogs(moduleSignalMap(m.name)) else m
    }
  }
}
*/

/*
object AspectModule {
  private[aspect] val dynamicContextVar = new DynamicVariable[Option[MonitorModule]](None)
  private[aspect] def withAspect[S](m: MonitorModule)(thunk: => S): S = {
    dynamicContextVar.withValue(Some(m))(thunk)
  }
  def getFirrtl(chiselIR: chisel3.internal.firrtl.Circuit): firrtl.ir.DefModule = {
    // Build FIRRTL AST
    val firrtlString = chisel3.internal.firrtl.Emitter.emit(chiselIR)
    val firrtlIR = firrtl.Parser.parse(firrtlString)
    firrtlIR.modules.head
  }
  def getAnnotations(chiselIR: Circuit, dut: AspectModule, connections: Seq[(Component, Component)], parent: BaseModule): Seq[ChiselAnnotation] = {

    val firrtlModule = getFirrtl(chiselIR)

    // Return Annotations
    Seq(
      AspectAnnotation(
        connections,
        parent.toNamed,
        //new AddInstance(dut.instName, firrtlModule.name),
        _ => (s: Statement) => Block(Seq(s, DefInstance(NoInfo, dut.instName, firrtlModule.name))),
        Seq(firrtlModule))
    ) ++ chiselIR.annotations
  }

  def apply[M<: MultiIOModule, T<:Data](instanceName: String,
                                        parent: M,
                                        f: Snippet[M, T]
                                       ): (MonitorModule, Seq[ChiselAnnotation]) = {
    val connections = (parent: M, dut: MonitorModule) => {
      dut.cmrComponent.toMap.mapValues(_()) ++ Map((parent.clock.toNamed, dut.instComponent.ref("clock")), (parent.reset.toNamed, dut.instComponent.ref("reset")))
    }
    apply(instanceName, parent, () => new MonitorModule(instanceName, parent).snip(f), connections)
  }

  def apply[M<: MultiIOModule, S<:AspectModule with RawModule](instanceName: String,
                                                               parent: M,
                                                               aspect: () => S,
                                                               connections: (M, S) => Map[Component, Component]
                                                              ): (S, Seq[ChiselAnnotation]) = {
    // Elaborate aspect
    val (chiselIR, dut) = Driver.elaborateAndReturn(aspect)
    val connects = connections(parent, dut)
    (dut, getAnnotations(chiselIR, dut, connects.toSeq, parent))
  }
}
*/

