package dev.capslock.scodecexercise.wasm.sections

import dev.capslock.scodecexercise.UnitTest
import scodec.bits.*

class FuncSectionTest extends UnitTest:
  describe("FuncSection") {
    it("should encode") {
      val funcSection = FuncSection(Vector(0))
      val result = FuncSection.codecWithSize(1).encode(funcSection).require
      // magic number is omitted because this class is for just payload
      result shouldBe hex"01 00"
    }
    it("should decode") {
      val bytes =
        hex"01 00" // Function Section(code 03), size 2, count 1, index 0
      val result = FuncSection.codecWithSize(1).decodeValue(bytes.bits).require
      result shouldBe FuncSection(Vector(0))
    }
  }
