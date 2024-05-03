package dev.capslock.scodecexercise.wasm

import scodec.{*, given}
import scodec.codecs.*

trait SectionPayload

case class Section(header: SectionHeader, payload: SectionPayload)

object Section:
  val codec: Codec[Section] = SectionHeader.codec
    .>>~ {
      case header @ SectionHeader(SectionCode.TypeSection, size) =>
        s"typeSection($size bytes)" | sections.TypeSection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]

      case header @ SectionHeader(SectionCode.FunctionSection, size) =>
        s"functionSection($size bytes)" | sections.FuncSection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]

      case header @ SectionHeader(SectionCode.CodeSection, size) =>
        s"codeSection($size bytes)" | sections.CodeSection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]

      case header @ SectionHeader(SectionCode.ExportSection, size) =>
        s"exportSection($size bytes)" | sections.ExportSection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]
    }
    .as[Section]

  val codecAll = codecs.vector(codec)
