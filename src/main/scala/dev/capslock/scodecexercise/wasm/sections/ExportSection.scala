package dev.capslock.scodecexercise.wasm
package sections

import util.Leb128
import types.Export
import scodec.*
import scodec.codecs.*

case class ExportSection(exports: Vector[Export]) extends SectionPayload

object ExportSection:
  def codecWithSize(size: Int): Codec[ExportSection] = fixedSizeBytes(
    size,
    vectorOfN(Leb128.codecInt, Export.codec),
  ).as[ExportSection]
