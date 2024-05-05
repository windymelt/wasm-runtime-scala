package dev.capslock.scodecexercise.wasm
package types

import scodec.*
import scodec.codecs.*

import util.Leb128

case class Memory(limits: Limits)

object Memory:
  val codec: Codec[Memory] =
    val decoder: Decoder[Memory] = (Leb128.codecInt :: Leb128.codecInt)
      .flatMap:
        case (1, min) =>
          Leb128.codecInt.map(max => Memory(Limits(min, Some(max))))
        case (_, min) =>
          Decoder.pure(Memory(Limits(min, None)))
      .as[Memory]

    val encoder: Encoder[Memory] = Encoder[Memory]: memory =>
      memory.limits.max match
        case Some(max) =>
          for
            flag    <- Leb128.codecInt.encode(1)
            minBits <- Leb128.codecInt.encode(memory.limits.min)
            maxBits <- Leb128.codecInt.encode(max)
          yield flag ++ minBits ++ maxBits
        case None =>
          for
            flag    <- Leb128.codecInt.encode(0)
            minBits <- Leb128.codecInt.encode(memory.limits.min)
          yield flag ++ minBits

    Codec(encoder, decoder)
  end codec
