package dev.capslock.scodecexercise.wasm
package function

import dev.capslock.scodecexercise.wasm.function.OpCode.I32Add
import dev.capslock.scodecexercise.wasm.util.Leb128
import scodec.*
import scodec.bits.*
import scodec.codecs.*
import Attempt.Successful

enum OpCode(val code: Byte):
  case End      extends OpCode(0x0b)
  case Call     extends OpCode(0x10)
  case LocalGet extends OpCode(0x20)
  case LocalSet extends OpCode(0x21)
  case I32Const extends OpCode(0x41)
  case I32Eqz   extends OpCode(0x45)
  case I32LE_U  extends OpCode(0x4d)
  case I32Add   extends OpCode(0x6a)

object OpCode:
  private val opMap = OpCode.values.view.map(op => op.code -> op).toMap
  def fromByte(byte: Byte): OpCode = opMap(byte)

  val codec: Codec[OpCode] = byte.xmap(fromByte, _.code)

enum Instruction(val code: OpCode):
  case End                  extends Instruction(OpCode.End)
  case Call(funcIdx: Int)   extends Instruction(OpCode.Call)
  case LocalGet(index: Int) extends Instruction(OpCode.LocalGet)
  case LocalSet(index: Int) extends Instruction(OpCode.LocalSet)
  case I32Const(i32: Int)   extends Instruction(OpCode.I32Const)
  case I32Eqz               extends Instruction(OpCode.I32Eqz)
  case I32LE_U              extends Instruction(OpCode.I32LE_U)
  case I32Add               extends Instruction(OpCode.I32Add)

  def opEnc: Attempt[BitVector] = OpCode.codec.encode(code)

object Instruction:
  val encoder = new Encoder[Instruction] {
    private def opEnc                 = OpCode.codec.encode
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(value: Instruction): Attempt[BitVector] = value match
      case in @ (Instruction.End | Instruction.I32Eqz | Instruction.I32LE_U |
          Instruction.I32Add) =>
        in.opEnc

      case Instruction.Call(funcIdx) =>
        for
          op   <- opEnc(OpCode.Call)
          func <- Leb128.codecInt.encode(funcIdx)
        yield op ++ func

      case Instruction.LocalGet(index) =>
        for
          op  <- opEnc(OpCode.LocalGet)
          idx <- Leb128.codecInt.encode(index)
        yield op ++ idx

      case Instruction.LocalSet(index) =>
        for
          op  <- opEnc(OpCode.LocalSet)
          idx <- Leb128.codecInt.encode(index)
        yield op ++ idx

      case Instruction.I32Const(x) =>
        for
          op    <- opEnc(OpCode.I32Const)
          const <- Leb128.codecInt.encode(x)
        yield op ++ const
  }

  val decoder = new Decoder[Instruction] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Instruction]] =
      for
        op  <- OpCode.codec.decode(bits)
        rem <- remain(op.value, op.remainder)
      yield rem

    private def remain(
        opCode: OpCode,
        bits: BitVector,
    ): Attempt[DecodeResult[Instruction]] =
      opCode match
        case OpCode.End => Successful(DecodeResult(Instruction.End, bits))

        case op @ (OpCode.Call | OpCode.LocalGet | OpCode.LocalSet |
            OpCode.I32Const) =>
          Leb128.codecInt
            .decode(bits)
            .map: x =>
              op match
                case OpCode.Call =>
                  DecodeResult(Instruction.Call(x.value), x.remainder)
                case OpCode.LocalGet =>
                  DecodeResult(Instruction.LocalGet(x.value), x.remainder)
                case OpCode.LocalSet =>
                  DecodeResult(Instruction.LocalSet(x.value), x.remainder)
                case OpCode.I32Const =>
                  DecodeResult(Instruction.I32Const(x.value), x.remainder)

        case OpCode.I32Eqz =>
          Successful(DecodeResult(Instruction.I32Eqz, bits))

        case OpCode.I32LE_U =>
          Successful(DecodeResult(Instruction.I32LE_U, bits))

        case OpCode.I32Add => Successful(DecodeResult(Instruction.I32Add, bits))
  }

  val codec: Codec[Instruction] = Codec(encoder, decoder)
