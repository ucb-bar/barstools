// See LICENSE for license details.

package beagleutils

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.annotations.{InstanceTarget, ModuleTarget, Annotation, SingleTargetAnnotation}
import firrtl.Mappers._
import firrtl.Utils._

import scala.collection.mutable.ArrayBuffer

class BeagleTransforms extends SeqTransform {
  def inputForm = MidForm
  def outputForm = MidForm

  def transforms = Seq(
    new ExtractToTop,
    //new ResolveAndCheck)
    new ResolveAndCheck,
    new firrtl.transforms.GroupAndDedup)
}
