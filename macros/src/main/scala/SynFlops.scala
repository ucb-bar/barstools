// See LICENSE for license details.

package barstools.macros

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.passes.MemPortUtils.{memPortField, memType}
import Utils._

class SynFlopsPass(synflops: Boolean, libs: Seq[Macro]) extends firrtl.passes.Pass {
  lazy val libMods = (libs map { lib => lib.src.name -> {
    val dataType = (lib.src.ports foldLeft (None: Option[BigInt]))((res, port) =>
      (res, port.maskPort) match {
        case (_, None) =>
          res
        case (None, Some(_)) =>
          Some(port.effectiveMaskGran)
        case (Some(x), Some(_)) =>
          assert(x == port.effectiveMaskGran)
          res
      }
    ) match {
      case None => UIntType(IntWidth(lib.src.width))
      case Some(gran) => VectorType(UIntType(IntWidth(gran)), (lib.src.width / gran).toInt)
    }

    val mem = DefMemory(
      NoInfo,
      "ram",
      dataType,
      lib.src.depth,
      1, // writeLatency
      1, // readLatency. This is possible because of VerilogMemDelays
      lib.readers.indices map (i => s"R$i"),
      lib.writers.indices map (i => s"W$i"),
      lib.readwriters.indices map (i => s"RW$i")
    )

    val readConnects = lib.readers.zipWithIndex flatMap { case (r, i) =>
      val clock = portToExpression(r.src.clock)
      val address = portToExpression(r.src.address)
      val enable = (r.src chipEnable, r.src readEnable) match {
        case (Some(en_port), Some(re_port)) =>
          and(portToExpression(en_port),
              portToExpression(re_port))
        case (Some(en_port), None) => portToExpression(en_port)
        case (None, Some(re_port)) => portToExpression(re_port)
        case (None, None) => one
      }
      val data = memPortField(mem, s"R$i", "data")
      val read = (dataType: @unchecked) match {
        case VectorType(tpe, size) => cat(((0 until size) map (k =>
          WSubIndex(data, k, tpe, UNKNOWNGENDER))).reverse)
        case _: UIntType => data
      }
      Seq(
        Connect(NoInfo, memPortField(mem, s"R$i", "clk"), clock),
        Connect(NoInfo, memPortField(mem, s"R$i", "addr"), address),
        Connect(NoInfo, memPortField(mem, s"R$i", "en"), enable),
        Connect(NoInfo, WRef(r.src.output.get.name), read)
      )
    }

    val writeConnects = lib.writers.zipWithIndex flatMap { case (w, i) =>
      val clock = portToExpression(w.src.clock)
      val address = portToExpression(w.src.address)
      val enable = (w.src.chipEnable, w.src.writeEnable) match {
        case (Some(en), Some(we)) =>
          and(portToExpression(en),
              portToExpression(we))
        case (Some(en), None) => portToExpression(en)
        case (None, Some(we)) => portToExpression(we)
        case (None, None) => zero // is it possible?
      }
      val mask = memPortField(mem, s"W$i", "mask")
      val data = memPortField(mem, s"W$i", "data")
      val write = portToExpression(w.src.input.get)
      Seq(
        Connect(NoInfo, memPortField(mem, s"W$i", "clk"), clock),
        Connect(NoInfo, memPortField(mem, s"W$i", "addr"), address),
        Connect(NoInfo, memPortField(mem, s"W$i", "en"), enable)
      ) ++ (dataType match {
        case VectorType(tpe, size) =>
          val width = bitWidth(tpe).toInt
          ((0 until size) map (k =>
            Connect(NoInfo, WSubIndex(data, k, tpe, UNKNOWNGENDER),
                            bits(write, (k + 1) * width - 1, k * width)))) ++
          ((0 until size) map (k =>
            Connect(NoInfo, WSubIndex(mask, k, BoolType, UNKNOWNGENDER),
                            bits(WRef(w.src.maskPort.get.name), k))))
        case _: UIntType =>
          Seq(Connect(NoInfo, data, write), Connect(NoInfo, mask, one))
      })
    }

    val readwriteConnects = lib.readwriters.zipWithIndex flatMap { case (rw, i) =>
      val clock = portToExpression(rw.src.clock)
      val address = portToExpression(rw.src.address)
      val wmode = rw.src.writeEnable match {
        case Some(we) => portToExpression(we)
        case None => zero // is it possible?
      }
      val enable = (rw.src.chipEnable, rw.src.readEnable) match {
        case (Some(en), Some(re)) =>
          and(portToExpression(en), or(portToExpression(re), wmode))
        case (Some(en), None) => portToExpression(en)
        case (None, Some(re)) => or(portToExpression(re), wmode)
        case (None, None) => one
      }
      val wmask = memPortField(mem, s"RW$i", "wmask")
      val wdata = memPortField(mem, s"RW$i", "wdata")
      val rdata = memPortField(mem, s"RW$i", "rdata")
      val write = portToExpression(rw.src.input.get)
      val read = (dataType: @unchecked) match {
        case VectorType(tpe, size) => cat(((0 until size) map (k =>
          WSubIndex(rdata, k, tpe, UNKNOWNGENDER))).reverse)
        case _: UIntType => rdata
      }
      Seq(
        Connect(NoInfo, memPortField(mem, s"RW$i", "clk"), clock),
        Connect(NoInfo, memPortField(mem, s"RW$i", "addr"), address),
        Connect(NoInfo, memPortField(mem, s"RW$i", "en"), enable),
        Connect(NoInfo, memPortField(mem, s"RW$i", "wmode"), wmode),
        Connect(NoInfo, WRef(rw.src.output.get.name), read)
      ) ++ (dataType match {
        case VectorType(tpe, size) =>
          val width = bitWidth(tpe).toInt
          ((0 until size) map (k =>
            Connect(NoInfo, WSubIndex(wdata, k, tpe, UNKNOWNGENDER),
                            bits(write, (k + 1) * width - 1, k * width)))) ++
          ((0 until size) map (k =>
            Connect(NoInfo, WSubIndex(wmask, k, BoolType, UNKNOWNGENDER),
                            bits(WRef(rw.src.maskPort.get.name), k))))
        case _: UIntType =>
          Seq(Connect(NoInfo, wdata, write), Connect(NoInfo, wmask, one))
      })
    }

    lib.module(Block(mem +: (readConnects ++ writeConnects ++ readwriteConnects)))
  }}).toMap

  def run(c: Circuit): Circuit = {
    if (!synflops) c
    else c.copy(modules = (c.modules map (m => libMods getOrElse (m.name, m))))
  }
}
