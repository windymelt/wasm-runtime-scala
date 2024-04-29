package dev.capslock.scodecexercise.wasm.util

import scodec.bits.BitVector
import scodec.{*, given}

// Codec for LEB128 encoding(Little Endian Base 128 Varints)
object Leb128:
  val codec: Codec[Long] = new Codec[Long] {
    def sizeBound: SizeBound = SizeBound.unknown

    def encode(value: Long): Attempt[BitVector] = {
      if (value < 0) {
        throw new IllegalArgumentException("LEB128 can't encode negative value")
      }
      val buffer = scala.collection.mutable.ArrayBuffer[Byte]()
      var v = value
      while (v != 0) {
        var byte = (v & 0x7f).toByte
        v >>= 7
        if (v != 0) {
          byte = (byte | 0x80).toByte
        }
        buffer += byte
      }
      Attempt.successful(BitVector(buffer.toArray))
    }

    def decode(bits: BitVector) = {
      var value = 0L
      var shift = 0
      var byte = 0
      var i = 0
      while {
        if (i >= bits.bytes.size) {
          false
        } else {
          byte = bits.bytes(i)
          i += 1
          value |= (byte & 0x7f).toLong << shift
          shift += 7
          (byte & 0x80) != 0
        }
      } do ()
      Attempt.successful(DecodeResult(value, bits.drop(i * 8)))
    }
  }

  val codecInt: Codec[Int] = codec.xmap(_.toInt, _.toLong)
