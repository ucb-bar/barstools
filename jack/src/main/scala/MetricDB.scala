// See LICENSE for license details
package barstools.jack

import scala.collection.mutable.HashMap

object MetricDB {

  val keys = new HashMap[String, Int]()

  def getNewKey(s: String): String = {
    val n = keys.getOrElse(s, 0)
    keys.update(s, n+1)
    return s"${s}_${n}"
  }

}

case class ClockSpec(path: Seq[String], name: String)
