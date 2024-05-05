package dev.capslock.scodecexercise.wasm
package types

import util.Leb128
import scodec.Codec
import scodec.codecs.*

case class Export(name: String, desc: ExportDesc)

enum ExportDesc(code: Int):
  case Func(idx: Int) extends ExportDesc(0x00)

object Export:
  val codec: Codec[Export] =
    def decoder(name: String, typ: Int, idx: Int) = {
      val desc = typ match
        case 0x00 => ExportDesc.Func(idx)
      Export(name, desc)
    }

    def encoder(ex: Export) = ex.desc match
      case ExportDesc.Func(idx) =>
        (ex.name, 0x00, idx)

    (variableSizeBytes(Leb128.codecInt, utf8) :: uint8L :: Leb128.codecInt)
      .xmap(
        decoder,
        encoder,
      )
  end codec
