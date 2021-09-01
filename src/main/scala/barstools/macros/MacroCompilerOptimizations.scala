// SPDX-License-Identifier: Apache-2.0

package barstools.macros

import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.{passes, DependencyAPIMigration, Emitter, SeqTransform, Transform}

class MacroCompilerOptimizations extends SeqTransform with DependencyAPIMigration {
  override def prerequisites:          Seq[TransformDependency] = Forms.LowForm
  override def optionalPrerequisites:  Seq[TransformDependency] = Forms.LowFormOptimized
  override def optionalPrerequisiteOf: Seq[Dependency[Emitter]] = Forms.LowEmitters
  override def invalidates(a: Transform) = false

  def transforms: Seq[Transform] = Seq(
    passes.RemoveValidIf,
    new firrtl.transforms.ConstantPropagation,
    passes.memlib.VerilogMemDelays,
    new firrtl.transforms.ConstantPropagation,
    passes.SplitExpressions,
    passes.CommonSubexpressionElimination
  )
}
