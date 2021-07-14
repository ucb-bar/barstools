// See LICENSE for license details
package barstools.floorplan.compiler

import scala.collection.Map
import scala.collection.mutable.HashMap
import java.io.File
import scala.io.Source

import firrtl.annotations.TargetToken.{Instance, OfModule}

object MemInstMap {

  def fromFiles(files: Seq[String]): Map[OfModule, Seq[(Instance, OfModule)]] = {
    val hashMap = new HashMap[OfModule, Seq[(Instance, OfModule)]]()
    files.foreach { f =>
      Source.fromFile(new File(f)).getLines.foreach { line: String =>
        val listRaw = line.split(" ")
        val module = OfModule(listRaw(0))
        hashMap += module -> listRaw.toSeq.drop(1).map { x =>
          val s = x.split(":")
          (Instance(s(0)), OfModule(s(1)))
        }
      }
    }
    hashMap.toMap
  }

}

