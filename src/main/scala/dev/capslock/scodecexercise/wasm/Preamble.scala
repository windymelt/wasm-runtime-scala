package dev.capslock.scodecexercise.wasm

import scodec.{*, given}
import scodec.codecs.{*, given}
import scodec.bits.*

case class Preamble(version: Long)

object Preamble:
  // version should be encodec in little endian
  val codec: Codec[Preamble] =
    // \0asm version
    logToStdOut(
      "preamble" | (constant(hex"0061 736d") :: uint32L).as[Preamble],
      "preamble",
    )
