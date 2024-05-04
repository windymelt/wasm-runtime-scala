package dev.capslock.scodecexercise.wasm
package types

import util.Leb128
import scodec.Codec
import scodec.codecs.*

case class Import(module: String, field: String, desc: ImportDesc)

enum ImportDesc(code: Int):
  case Func(idx: Int) extends ImportDesc(0x00)

object Import:
  val codec: Codec[Import] =
    def decoder(module: String, field: String, typ: Int, idx: Int): Import =
      typ match
        case 0x00 => Import(module, field, ImportDesc.Func(idx))

    def encoder(im: Import) = im.desc match
      case ImportDesc.Func(idx) =>
        (im.module, im.field, 0x00, idx)

    (variableSizeBytes(Leb128.codecInt, utf8)
      :: variableSizeBytes(
        Leb128.codecInt,
        utf8,
      ) :: uint8L :: Leb128.codecInt)
      .xmap(decoder, encoder)
  end codec
