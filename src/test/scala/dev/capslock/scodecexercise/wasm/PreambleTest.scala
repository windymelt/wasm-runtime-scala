package dev.capslock.scodecexercise.wasm

import dev.capslock.scodecexercise.UnitTest
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scodec.{*, given}
import scodec.bits.*
import scodec.codecs.*

class PreambleTest extends UnitTest:
  describe("Preamble") {
    it("should be able to parse preamble") {
      val expectedWasm = wat2wasm("(module)")
      val preamble     = Preamble(version = 1)
      val encoded      = Preamble.codec.encode(preamble).require
      encoded shouldBe BitVector(expectedWasm)
      val decoded = Preamble.codec.decode(encoded).require
      decoded.value shouldBe preamble
    }
  }
