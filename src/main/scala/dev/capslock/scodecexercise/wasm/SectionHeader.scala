package dev.capslock.scodecexercise.wasm

import scodec.*
import scodec.codecs.*

case class SectionHeader(sectionCode: SectionCode, sectionSize: Long)

object SectionHeader:
  val codec: Codec[SectionHeader] = {
    logToStdOut(
      "sectionHeader" | (SectionCode.codec :: util.Leb128.codec)
        .as[SectionHeader],
      "sectionHeader",
    )
  }
