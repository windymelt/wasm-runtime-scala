package dev.capslock.scodecexercise.wasm.types

import dev.capslock.scodecexercise.wasm.util.Leb128
import scodec.bits.hex
import scodec.{*, given}
import scodec.codecs.*

case class FuncType(params: Vector[ValueType], results: Vector[ValueType])

object FuncType:
  val codec: Codec[FuncType] = {
    val paramsCodec = vectorOfN(Leb128.codecInt, ValueType.codec)
    val resultsCodec = vectorOfN(Leb128.codecInt, ValueType.codec)
    "funcType" | ("magic (funcType)" | constant(
      hex"60",
    ) :: ("params" | paramsCodec) :: ("results" | resultsCodec))
      .as[FuncType]
  }
