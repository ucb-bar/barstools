package transforms

import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}
import chisel3.internal.InstanceId
import firrtl.Transform
import firrtl.annotations.Annotation

final case class LagacyAnnotation(
                             component: InstanceId,
                             transformClass: Class[_ <: Transform],
                             value: String) extends ChiselAnnotation with RunFirrtlTransform {
  def toFirrtl: Annotation = Annotation(component.toNamed, transformClass, value)
}
