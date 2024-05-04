package dev.capslock.scodecexercise.wasm
package sections

import util.Leb128
import scodec.{*, given}
import scodec.codecs.*

case class FuncSection(functionTypeIndices: Vector[Int]) extends SectionPayload

object FuncSection:
  def codecWithSize(size: Int): Codec[FuncSection] =
    fixedSizeBytes(size, vectorOfN(Leb128.codecInt, Leb128.codecInt))
      .as[FuncSection]
