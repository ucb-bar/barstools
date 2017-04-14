// See LICENSE for license details

package barstools.tapeout.transforms

import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import chisel3.testers.BasicTester
import chisel3.experimental.{Analog, attach}
import firrtl.ir.{AnalogType, Circuit, DefModule, DefWire, Expression, HasName, Port, Statement, Type}
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.annotations.{Annotation, ModuleName, Named, ComponentName}
import firrtl.Mappers._

object AnalogRenamerAnnotation {
  def apply(target: Named, value: String): Annotation =
    Annotation(target, classOf[AnalogRenamer], value)

  def unapply(a: Annotation): Option[(ComponentName, String)] = a match {
    case Annotation(named, t, value) if t == classOf[AnalogRenamer] => named match {
      case c: ComponentName => Some((c, value))
      case _ => None
    }
    case _ => None
  }
}

class AnalogRenamer extends Transform {
  override def inputForm: CircuitForm  = LowForm
  override def outputForm: CircuitForm = LowForm

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => state
      case annos =>
        val analogs = annos.collect { case AnalogRenamerAnnotation(ana, name) => (ana, name) }
        state.copy(circuit = run(state.circuit, analogs))
    }
  }

  def run(circuit: Circuit, annos: Seq[(ComponentName, String)]): Circuit = {
    circuit map walkModule(annos)
  }
  def walkModule(annos: Seq[(ComponentName, String)])(m: DefModule): DefModule = {
    val filteredAnnos = Map(annos.filter(a => a._1.module.name == m.name).map {
      case (c, s) => 
        c.name.replace(".", "_") -> s
    }: _*)
    m map walkStatement(filteredAnnos) map walkPort(filteredAnnos)
  }
  def walkStatement(annos: Map[String, String])(s: Statement): Statement = {
    val visited = s map walkStatement(annos)
    visited match {
      case w: DefWire =>
        if (annos.contains(w.name)) {
          updateAnalogVerilog(annos(w.name))(w.tpe)
        }
        w
      case _ => visited
    }
  }
  def walkPort(annos: Map[String, String])(p: Port): Port = {
    if (annos.contains(p.name)) {
      updateAnalogVerilog(annos(p.name))(p.tpe)
    }
    p
  }
  def updateAnalogVerilog(value: String)(tpe: Type): Type = {
    tpe match {
      case a: AnalogType =>
        a.verilogTpe = value
        a
      case t => t
    }
  }
}

trait AnalogAnnotator { self: Module =>
  def renameAnalog(component: Analog, value: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[AnalogRenamer], value))
  }
}
