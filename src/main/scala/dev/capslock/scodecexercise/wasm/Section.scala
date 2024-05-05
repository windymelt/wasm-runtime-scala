package dev.capslock.scodecexercise.wasm

import scodec.{*, given}
import scodec.codecs.*

trait SectionPayload

case class Section(header: SectionHeader, payload: SectionPayload)

object Section:
  val codec: Codec[Section] = SectionHeader.codec
    .flatZip {
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

      case header @ SectionHeader(SectionCode.ImportSection, size) =>
        s"importSection($size bytes)" | sections.ImportSection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]

      case header @ SectionHeader(SectionCode.MemorySection, size) =>
        s"memorySection($size bytes)" | sections.MemorySection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]

      case header @ SectionHeader(SectionCode.DataSection, size) =>
        s"dataSection($size bytes)" | sections.DataSection
          .codecWithSize(size.toInt)
          .upcast[SectionPayload]
    }
    .as[Section]

  val codecAll: Codec[Vector[Section]] = codecs.vector(codec)
