package barstools.macros

import mdf.macrolib._

class SRAMCompiler extends MacroCompilerSpec with HasSRAMGenerator {
  val compiler = generateSRAMCompiler("awesome", "A")
  val lib = s"lib-SRAMCompiler.json"
  val verilog = s"v-SRAMCompiler.v"
  writeToLib(lib, Seq(compiler))

  val mem = s"mem-SRAMCompiler.json"
  writeToMem(mem, Seq(generateSRAM("mymem", "X", 8, 16)))

  compile(mem, Some(lib), verilog, false, true)
}
