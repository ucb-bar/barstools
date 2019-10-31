package aoplib.coverage

import chisel3._
import firrtl.RenameMap

//trait CoverBase { val label: String }


object CoverPoint {
  def apply(label: String, signal: Bits, bins: Seq[BaseBin], pointOptions: CoverOptions = CoverOptions()): CoverPoint = {
    CoverPoint(label, SignalTracker(signal), bins, pointOptions)
  }
}
case class CoverPoint private (label: String,
                          signal: SignalTracker,
                          bins: Seq[BaseBin],
                          pointOptions: CoverOptions) {
  def update(renames: RenameMap): CoverPoint = {
    this.copy(signal = signal.singleUpdate(renames))
  }

  /*
  def generateChisel(clock: Clock, reset: Reset): Unit = {
    val sig = signal.signal.asUInt()
    withClockAndReset(clock, reset) {
      val defaults = bins.collect { case b@Bin(_, Default) => b }
      assert(defaults.size <= 1, s"Coverpoint $label on signal ${signal.signal.toTarget} can no more than one default bin.")
      val inRanges = bins.map {
        case Bin(label, BinRange(low, high)) =>
          val (counter, willWrap) = util.Counter(sig >= low.U & sig <= high.U, pointOptions.maxCount)
          counter.suggestName(label)
          when(willWrap) {
            counter := counter
          }
      }

    }

  }
  */
}
case class CoverOptions(weights: Seq[Int] = Seq(1), maxCount: Int = 32)

// TODO: I think you can get away without representing this directly and generating it programmatically
// case class CrossPoint(name: String, points: Seq[CoverPoint], bins: Seq[BaseBin]) extends CoverBase

abstract class BaseBin {
  val labelOption: Option[String]
  val category: BinCategory
}

// Explicit bin, bins based on category
case class Bin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}

// Implicit bin, bins based on category
// Not user created
case class ImplicitBin(category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = None
}

// Ignores when bin matches (usually paired with ImplicitBin
case class IgnoreBin(label: String, category: BinCategory) extends BaseBin {
  override val labelOption: Option[String] = Some(label)
}


trait BinCategory

// Defaults to all non-specified categories
case object Default extends BinCategory

// Low and High are inclusive
case class BinRange(low: BigInt, high: BigInt) extends BinCategory

// A sequence of values that must be transitioned to, in order
// Wait on this...
//case class BinTransition(sequence: Seq[BinValue]) extends BinCategory

// Unnecessary!
//trait BinValue

// A value in a sequence that must match immediately
//case class BinConstant(value: BigInt) extends BinValue

// A value that must be hit eventually, but not necessarily at this time
//case class BinEventually(value: BigInt) extends BinValue


