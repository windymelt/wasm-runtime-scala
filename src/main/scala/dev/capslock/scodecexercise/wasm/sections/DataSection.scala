package dev.capslock.scodecexercise.wasm
package sections

import util.Leb128
import types.Data

import scodec.*
import scodec.codecs.*

case class DataSection(data: Vector[Data]) extends SectionPayload

object DataSection:
  def codecWithSize(size: Int): Codec[DataSection] = fixedSizeBytes(
    size,
    vectorOfN(Leb128.codecInt, Data.codec),
  ).as[DataSection]
