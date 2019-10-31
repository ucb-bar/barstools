package aoplib.histogram

import chisel3._
import chisel3.aop._
import chisel3.aop.injecting.{InjectingAspect, InjectingTransform}
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.util.experimental.BoringUtils
import firrtl.annotations.Annotation
import firrtl.passes.wiring.WiringTransform
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.{AnnotationSeq, Transform}

import scala.reflect.runtime.universe.TypeTag

/** Create histograms of signal values during execution, and print the bin values at the end of execution
  *
  * @param selectRoots Given top-level module, pick the instances of a module to apply the aspect (root module)
  * @param selectSignals Pick signals from a module to histogram
  * @param selectDesignDone From the top-level design, select a signal which, when true, the design has finished execution
  * @param selectSimDone From the top-level design, select an assignable signal which, when true, the simulation has finished execution
  * @param tTag Needed to prevent type-erasure of the top-level module type
  * @param mTag Needed to prevent type-erasure of the selected modules' type
  * @tparam T Type of top-level module
  * @tparam M Type of root module (join point)
  */
case class HistogramAspect[T <: RawModule, M <: RawModule](selectRoots: T => Seq[M],
                                                           selectSignals: M => Seq[HistogramSignal],
                                                           selectDesignDone: T => Bool,
                                                           selectSimDone: T => Bool)
                                                          (implicit tTag: TypeTag[T], mTag: TypeTag[M]) extends Aspect[T] {
  private final def markDone(d: Data): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = firrtl.passes.wiring.SourceAnnotation(d.toTarget, "histogramDone")
    })
  }

  private final def setDone(d: Data): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = firrtl.passes.wiring.SinkAnnotation(d.toNamed, "histogramDone")
    })
  }

  final def toAnnotation(dut: T): AnnotationSeq = {
    // Create annotation to insert histogram into module
    val ia = InjectingAspect[T, M](
      selectRoots,
      { m: M =>
        val signals = selectSignals(m)
        val done = Wire(Bool())
        done := DontCare
        BoringUtils.bore(selectDesignDone(dut), Seq(done))
        val histograms = signals.map { s =>
          val histogram = Module(new Histogram(s, s.signal.name, m.name))
          histogram.in := s.signal
          histogram.setHistogramming := true.B
          histogram.setReadingOut := false.B
          histogram.allDone := false.B
          histogram
        }

        val allDone = WireInit(false.B)

        when(done) {
          histograms.foreach { h =>
            h.setHistogramming := false.B
          }
          allDone := histograms.foldLeft(1.U) { (readOut, h) =>
            h.setReadingOut := readOut
            val regNext = RegNext(h.doneReading)
            when(regNext) {
              h.setReadingOut := false.B
              h.allDone := true.B
            }

            readOut & h.doneReading
          }
        }

        dontTouch(allDone)
        markDone(allDone)
      }
    ).toAnnotation(dut)

    // Create annotation to insert histogram execution after design execution
    val ia2 = InjectingAspect({dut: T => Seq(dut)}, { dut: T => setDone(selectSimDone(dut)) }).toAnnotation(dut)

    ia ++ ia2 ++ Seq(RunFirrtlTransformAnnotation(new InjectingTransform), RunFirrtlTransformAnnotation(new WiringTransform))
  }
}
