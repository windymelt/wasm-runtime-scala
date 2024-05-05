package dev.capslock.scodecexercise.wasm
package sections

import dev.capslock.scodecexercise.wasm.util.Leb128
import scodec.*
import scodec.codecs.*
import types.Memory

case class MemorySection(mem: Vector[Memory]) extends SectionPayload

object MemorySection:
  def codecWithSize(size: Int): Codec[MemorySection] =
    fixedSizeBytes(
      size,
      vectorOfN(Leb128.codecInt, Memory.codec),
    ).as[MemorySection]
