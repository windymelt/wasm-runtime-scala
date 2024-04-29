package dev.capslock.scodecexercise.wasm.types

import dev.capslock.scodecexercise.UnitTest
import scodec.bits.hex

class FuncTypeTest extends UnitTest:
  describe("FuncType") {
    it("should decode") {
      val bytes = hex"60 02 7f 7f 01 7e".bits
      val funcType = FuncType.codec.decode(bytes).require.value
      funcType shouldBe FuncType(
        Vector(ValueType.I32, ValueType.I32),
        Vector(ValueType.I64),
      )
    }

    it("should encode") {
      val funcType = FuncType(
        Vector(ValueType.I32, ValueType.I32, ValueType.I64),
        Vector(ValueType.I64, ValueType.I32),
      )
      val bytes = FuncType.codec.encode(funcType).require
      bytes shouldBe hex"60 03 7f 7f 7e 02 7e 7f".bits
    }
  }
