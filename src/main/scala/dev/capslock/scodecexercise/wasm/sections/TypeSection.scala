package dev.capslock.scodecexercise.wasm
package sections

import dev.capslock.scodecexercise.wasm.util.Leb128
import scodec.{*, given}
import scodec.codecs.*
import types.FuncType

case class TypeSection(types: Vector[FuncType]) extends SectionPayload

object TypeSection:
  def codecWithSize(size: Int): Codec[TypeSection] =
    logToStdOut(
      fixedSizeBytes(size, vectorOfN(Leb128.codecInt, FuncType.codec))
        .as[TypeSection],
      "typeSection",
    )
