package dev.capslock.scodecexercise.wasm

import scodec.*

case class WasmBinary(preamble: Preamble, sections: Vector[Section])

object WasmBinary:
  val codec: Codec[WasmBinary] = {
    val preamble = Preamble.codec
    val sections = Section.codecAll
    (preamble :: sections).as[WasmBinary]
  }
