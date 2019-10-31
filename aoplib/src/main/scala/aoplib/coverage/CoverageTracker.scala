package aoplib.coverage

import aoplib.histogram.HistogramSignal
import chisel3._

/** Either records and bins the value of [in] every cycle, or cycles through all bins and prints the result
  *
  * @param hinfo contains signal (and its type) as well as other histogramming information
  * @param name name of the instance of the parent module containing the signal
  * @param module name of the parent module containing the signal
  */
class CoverageTracker(hinfo: HistogramSignal, name: String, module: String) extends MultiIOModule {
  val in = IO(Input(chiselTypeOf(hinfo.signal)))
  val recording = IO(Input(Bool()))
  val printCoverage = IO(Input(Bool()))

  val inU = in.asUInt()

  val lows = VecInit(hinfo.intervals.map(_._2.U))
  val highs = VecInit(hinfo.intervals.map(_._3.U))

  // Calculate in's address into histogram
  val activeBinAddress = hinfo.intervals.zipWithIndex.foldLeft(0.U) {
    case (addr: UInt, ((_, min: Int, max: Int), index: Int)) => Mux((inU >= min.U) & (inU < max.U), index.U, addr)
  }

  // Instantiate coverage bins memory
  val coverbins = Mem(math.pow(2, activeBinAddress.getWidth).toInt, chiselTypeOf(hinfo.maxCount.U))

  // Records which bins have been written to (and which require initialization)
  val hasWritten = RegInit(VecInit(Seq.fill(coverbins.length.toInt)(false.B)))

  // Represents the address of the current bin our input signal is falling into
  val activeBinValue = Wire(chiselTypeOf(hinfo.maxCount.U))
  when(hasWritten(activeBinAddress)) {
    activeBinValue := coverbins.read(activeBinAddress)
  }.otherwise {
    activeBinValue := 0.U
  }

  // Then, do stuff
  when(reset.asBool() === false.B) {
    when(recording) {
      val writeValue = (activeBinValue + 1.U).min(hinfo.maxCount.U)
      coverbins.write(activeBinAddress, writeValue)
      hasWritten(activeBinAddress) := true.B
    }

    when(printCoverage) {
      val (message, expsAll) = hinfo.intervals.zipWithIndex.foldLeft((s"Coverage of $name in module $module:\n", Seq.empty[Bits])) {
        case ((str, exps), ((nameOpt, lo, hi), index)) =>
          /*
          val start = if (hinfo.label.isDefined) {
            s"    ${hinfo.label.get}:"
          } else s"    $index:"
          */
          val start = "    "
          val (strRest, expsRest) = if (nameOpt.isDefined) {
            (start + s"Bin ${nameOpt.get} ($lo to $hi) -> %d\n", Seq(coverbins.read(index.U)))
          } else {
            (start + s"Bin $index ($lo to $hi) -> %d\n", Seq(coverbins.read(index.U)))
          }
          (str + strRest, exps ++ expsRest)
      }
      printf(message, expsAll:_*)
    }
  }
}

/*

covergroup address_cov (ref logic [7:0] address,
  22        input int low, int high) @ (posedge ce);
  23     ADDRESS : coverpoint address {
  24       bins low    = {0,low};
  25       bins med    = {low,high};
  26     }
  27   endgroup
  28   //=================================================
  29   // Instance of covergroup
  30   //=================================================
  31   address_cov acov_low  = new(addr,0,10);
  32   address_cov acov_med  = new(addr,11,20);
  33   address_cov acov_high = new(addr,21,30);

 ===========================================================
 Group : coverage_covergroup.miff::address_cov
 ===========================================================
 SCORE  WEIGHT GOAL
 100.00 1      100
 -----------------------------------------------------------
 Summary for Group   coverage_covergroup.miff::address_cov
 CATEGORY  EXPECTED UNCOVERED COVERED PERCENT
 Variables 2        0         2       100.00

 Variables for Group  coverage_covergroup.miff::address_cov

 VARIABLE EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
 ADDRESS  2        0         2       100.00  100  1
 -----------------------------------------------------------
 Summary for Variable ADDRESS

 CATEGORY          EXPECTED UNCOVERED COVERED PERCENT
 User Defined Bins 2        0         2       100.00

 User Defined Bins for ADDRESS
 Bins

 NAME COUNT AT LEAST
 med  2     1
 low  6     1
 */

/*
covergroup datac @ (negedge cif.cb.ce);
  83      data_in : coverpoint cif.cb.datai {
  84        bins low    = {0,50};
  85        bins med    = {51,150};
  86        bins high   = {151,255};
  87      }
  88      data_out : coverpoint cif.cb.datao {
  89        bins low    = {0,50};
  90        bins med    = {51,150};
  91        bins high   = {151,255};
  92      }
  93      read_write : coverpoint cif.cb.we {
  94        bins  read  = {0};
  95        bins  write = {1};
  96      }
  97   endgroup

 VARIABLE EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
 address  3        2         1       33.33   100  1

 VARIABLE   EXPECTED UNCOVERED COVERED PERCENT GOAL WEIGHT
 data_in    3        2         1       33.33   100  1
 data_out   3        3         0       0.00    100  1
 read_write 2        0         2       100.00  100  1
 */
