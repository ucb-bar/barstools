// See LICENSE for license details
package barstools.jack

sealed abstract case class MetricNamespace(val sort: Int, val name: String)
final object HeuristicNamespace extends MetricNamespace(0, "heuristic")
final object SynthesisNamespace extends MetricNamespace(1, "synthesis")
final object PostPlaceNamespace extends MetricNamespace(2, "postplace")
final object PostRouteNamespace extends MetricNamespace(3, "postroute")
final object TimerToolNamespace extends MetricNamespace(4, "timertool")
final object PowerToolNamespace extends MetricNamespace(5, "powertool")

trait HasMetricMetadata {
  val namespace: String
  val key: String
}

