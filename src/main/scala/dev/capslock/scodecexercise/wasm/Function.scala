package dev.capslock.scodecexercise.wasm

import types.ValueType
import util.Leb128
import scodec.*
import scodec.codecs.*
import function.*

type ValueSize = Int
case class Function(
    locals: Vector[(ValueSize, ValueType)],
    body: Vector[Instruction],
)

object Function:
  val codec: Codec[Function] = {
    // Function ::= size, locals*, body*

    val size = logToStdOut(Leb128.codecInt, "Function size")
    val locals = logToStdOut(
      vectorOfN(Leb128.codecInt, Leb128.codecInt :: ValueType.codec),
      "Function locals",
    )
    val body = logToStdOut(vector(Instruction.codec), "Function body")

    variableSizeBytes(size, locals :: body).as[Function]
  }
