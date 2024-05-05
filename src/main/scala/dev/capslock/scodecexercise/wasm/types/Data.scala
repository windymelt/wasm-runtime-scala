package dev.capslock.scodecexercise.wasm
package types

import util.Leb128
import scodec.*
import scodec.bits.ByteVector
import scodec.codecs.*

case class Data(memoryIndex: Int, offset: Int, init: Array[Byte])

object Data:
  private val exprCodec: Codec[Int] =
    (Leb128.codecInt.unit(0) :: Leb128.codecInt :: Leb128.codecInt.unit(0))
      .xmap(
        { case (_, offset, _) => offset },
        { offset => ((), offset, ()) },
      )
      .as[Int]

  val codec: Codec[Data] =
    (Leb128.codecInt :: exprCodec :: Leb128.codecInt)
      .flatZip((memoryIndex, offset, initSize) =>
        bytes(initSize).xmap[Data](
          bs => Data(memoryIndex, offset, bs.toArray),
          d => ByteVector(d.init),
        ),
      )
      .xmap[Data](
        _._2,
        d => ((d.memoryIndex, d.offset, d.init.length), d),
      )
