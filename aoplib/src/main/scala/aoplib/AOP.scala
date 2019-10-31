package aoplib

import firrtl.RenameMap
import firrtl.annotations.ReferenceTarget

object Main extends App {
  println("Hello world.")
}

object AOP {
  def doStuff = println("Doing stuff")
}

object AnnotationHelpers {
  def renameOne(rt: ReferenceTarget, renames: RenameMap): ReferenceTarget = {
    renames.get(rt) match {
      case Some(s@Seq(x: ReferenceTarget)) => x
      case None => rt
      case x => sys.error(s"Cannot update $this when $rt is renamed to $x.")
    }
  }
  def renameMany(rts: Seq[ReferenceTarget], renames: RenameMap): Seq[ReferenceTarget] = {
    rts.flatMap { t =>
      renames.get(t) match {
        case Some(seq) => seq.map {
          case x: ReferenceTarget => x
          case x => sys.error(s"Cannot update $this when $t is renamed to $x.")
        }
        case None => Seq(t)
      }
    }
  }

}