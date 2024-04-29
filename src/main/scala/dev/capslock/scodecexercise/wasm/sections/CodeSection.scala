package dev.capslock.scodecexercise.wasm
package sections

import dev.capslock.scodecexercise.wasm.util.Leb128
import scodec.*
import scodec.codecs.*

case class CodeSection(functions: Vector[Function]) extends SectionPayload

object CodeSection:
  def codecWithSize(size: Int): Codec[CodeSection] = {
    fixedSizeBytes(
      size,
      vectorOfN(Leb128.codecInt, Function.codec),
    ).as[CodeSection]
  }
