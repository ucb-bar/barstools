package aoplib.histogram

import chisel3._

/** Specifies signal whose values will be histogrammed after execution
  *
  * Can overwrite functions to customize the histogram behavior
  *
  * @param signal Signal to histogram
  */
class HistogramSignal(val signal: Bits) {
  def maxCount = 100
  def minValue: Int = signal match {
    case _: UInt => 0
    case s: SInt => Math.pow(2, s.getWidth - 1).toInt
  }
  /* Until max is the smallest illegal value, or the max legal value plus 1 */
  def untilMax: Int = signal match {
    case u: UInt => Math.pow(2, u.getWidth).toInt
    case s: SInt => Math.pow(2, s.getWidth - 1).toInt
  }
  def nBins: Int = untilMax - minValue
  def ticks: Seq[Int] = {
    val binInterval = (untilMax - minValue) / nBins
    assert(binInterval * nBins + minValue == untilMax,
      s"nBins ${nBins} must divide evenly into the range from ${minValue} until ${untilMax}")
    val range = Range(minValue, untilMax + 1, binInterval)
    range.toList
  }
  def intervals: Seq[(Option[String], Int, Int)] = ticks.zip(ticks.tail).map { case (lo, hi) => (None, lo, hi) }
  def label: Option[String] = None
}
