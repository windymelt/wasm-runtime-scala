package dev.capslock.scodecexercise.wasm
package sections

import util.Leb128
import types.Import
import scodec.*
import scodec.codecs.*

case class ImportSection(imports: Vector[Import]) extends SectionPayload

object ImportSection:
  def codecWithSize(size: Int) = fixedSizeBytes(
    size,
    vectorOfN(Leb128.codecInt, Import.codec),
  ).as[ImportSection]
