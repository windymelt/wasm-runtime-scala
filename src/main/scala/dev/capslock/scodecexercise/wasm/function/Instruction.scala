package dev.capslock.scodecexercise.wasm
package function

import dev.capslock.scodecexercise.wasm.function.OpCode.I32Add
import dev.capslock.scodecexercise.wasm.util.Leb128
import scodec.*
import scodec.bits.*
import scodec.codecs.*
import types.ValueType

enum OpCode(val code: Byte):
  case End extends OpCode(0x0b)
  case LocalGet extends OpCode(0x20)
  case I32Const extends OpCode(0x41)
  case I32Add extends OpCode(0x6a)

object OpCode:
  def fromByte(byte: Byte): OpCode = byte match
    case 0x0b => End
    case 0x20 => LocalGet
    case 0x41 => I32Const
    case 0x6a => I32Add
    case _    => throw new IllegalArgumentException(s"Unknown opcode: $byte")

  val codec: Codec[OpCode] = byte.xmap(fromByte, _.code)

enum Instruction(val code: OpCode):
  case End extends Instruction(OpCode.End)
  case LocalGet(index: Int) extends Instruction(OpCode.LocalGet)
  case I32Const(i32: Int) extends Instruction(OpCode.I32Const)
  case I32Add extends Instruction(OpCode.I32Add)

object Instruction:
  val encoder = new Encoder[Instruction] {
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(value: Instruction): Attempt[BitVector] = value match
      case Instruction.End => OpCode.codec.encode(OpCode.End)
      case Instruction.LocalGet(index) =>
        OpCode.codec
          .encode(OpCode.LocalGet)
          .flatMap(op => Leb128.codecInt.encode(index).map(op ++ _))
      case Instruction.I32Const(x) =>
        for
          op <- OpCode.codec.encode(OpCode.I32Const)
          const <- Leb128.codecInt.encode(x) // XXX: I32 literal?
        yield op ++ const
      case Instruction.I32Add => OpCode.codec.encode(OpCode.I32Add)
  }

  val decoder = new Decoder[Instruction] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Instruction]] =
      OpCode.codec.decode(bits).flatMap { opCode =>
        opCode.value match
          case OpCode.End =>
            Attempt.successful(DecodeResult(Instruction.End, BitVector.empty))
          case OpCode.LocalGet =>
            Leb128.codecInt.decode(opCode.remainder).map { index =>
              DecodeResult(Instruction.LocalGet(index.value), index.remainder)
            }
          case OpCode.I32Const =>
            Leb128.codecInt.decode(opCode.remainder).map { x =>
              DecodeResult(Instruction.I32Const(x.value), BitVector.empty)
            }
          case OpCode.I32Add =>
            Attempt.successful(
              DecodeResult(Instruction.I32Add, BitVector.empty),
            )
      }
  }
  val codec: Codec[Instruction] = Codec(encoder, decoder)
