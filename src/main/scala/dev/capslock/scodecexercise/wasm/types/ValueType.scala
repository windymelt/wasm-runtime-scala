package dev.capslock.scodecexercise.wasm.types

import scodec.*
import scodec.codecs.*

enum ValueType:
  case I32, I64

object ValueType:
  def fromByte(byte: Byte): ValueType = byte match
    case 0x7f => I32
    case 0x7e => I64
    case _ =>
      throw new IllegalArgumentException(
        s"Invalid byte value for ValueType: $byte",
      )

  def toByte(valueType: ValueType): Byte = valueType match
    case I32 => 0x7f
    case I64 => 0x7e

  val codec: Codec[ValueType] =
    logToStdOut(byte.xmap(fromByte, toByte), "valueType")
