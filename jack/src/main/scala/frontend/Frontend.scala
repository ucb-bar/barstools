// See LICENSE for license details
package barstools.jack.frontend

import barstools.jack._
import chisel3.{Module, Clock}
import chisel3.experimental.{annotate, ChiselAnnotation, RawModule}

object ModuleArea {
  def apply[T <: RawModule](m: T, key: String = "", namespace: MetricNamespace = SynthesisNamespace): Unit = {
    annotate(new ChiselAnnotation { def toFirrtl: ModuleAreaAnnotation = ModuleAreaAnnotation(m.toNamed.toTarget, MetricDB.getNewKey(key), namespace.name) })
  }
}

object CriticalPath {
  def apply[T <: Module](m: T): Unit = CriticalPath(m, m.clock, "", SynthesisNamespace)
  def apply[T <: Module](m: T, key: String): Unit = CriticalPath(m, m.clock, key, SynthesisNamespace)
  def apply[T <: Module](m: T, namespace: MetricNamespace): Unit = CriticalPath(m, m.clock, "", namespace)
  def apply[T <: Module](m: T, key: String, namespace: MetricNamespace): Unit = CriticalPath(m, m.clock, key, namespace)
  def apply[T <: RawModule](m: T, clock: Clock): Unit = CriticalPath(m, clock, "", SynthesisNamespace)
  def apply[T <: RawModule](m: T, clock: Clock, key: String): Unit = CriticalPath(m, clock, key, SynthesisNamespace)
  def apply[T <: RawModule](m: T, clock: Clock, namespace: MetricNamespace): Unit = CriticalPath(m, clock, "", namespace)
  def apply[T <: RawModule](m: T, clock: Clock, key: String, namespace: MetricNamespace): Unit = {
    annotate(new ChiselAnnotation { def toFirrtl: CriticalPathAnnotation = CriticalPathAnnotation(m.toNamed.toTarget, MetricDB.getNewKey(key), namespace.name, clock.toNamed.toTarget) })
  }
}
