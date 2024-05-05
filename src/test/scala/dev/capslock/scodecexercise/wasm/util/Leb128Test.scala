package dev.capslock.scodecexercise.wasm
package util

import dev.capslock.scodecexercise.UnitTest
import scodec.bits.BitVector

class Leb128Test extends UnitTest:
  describe("LEB128") {
    it("should encode 1") {
      val encoded  = Leb128.codec.encode(1).require
      val expected = BitVector(Array[Byte](0x01.toByte))
      encoded shouldBe expected

      val decoded = Leb128.codec.decodeValue(encoded).require
      decoded shouldBe 1
    }
    it("should encode 127") {
      val encoded  = Leb128.codec.encode(127).require
      val expected = BitVector(Array[Byte](0x7f.toByte))
      encoded shouldBe expected

      val decoded = Leb128.codec.decodeValue(encoded).require
      decoded shouldBe 127
    }
    it("should encode 130") {
      val encoded  = Leb128.codec.encode(130).require
      val expected = BitVector(Array[Byte](0x82.toByte, 0x01.toByte))
      encoded shouldBe expected

      val decoded = Leb128.codec.decodeValue(encoded).require
      decoded shouldBe 130
    }
    it("should encode 624485") {
      val encoded = Leb128.codec.encode(624485).require
      val expected =
        BitVector(Array[Byte](0xe5.toByte, 0x8e.toByte, 0x26.toByte))
      encoded shouldBe expected

      val decoded = Leb128.codec.decodeValue(encoded).require
      decoded shouldBe 624485
    }
  }

  describe("LEB128.codecInt") {
    it("should encode 127") {
      val encoded  = Leb128.codecInt.encode(127).require
      val expected = BitVector(Array[Byte](0x7f.toByte))
      encoded shouldBe expected

      val decoded = Leb128.codecInt.decodeValue(encoded).require
      decoded shouldBe 127
    }
    it("should encode 624485") {
      val encoded = Leb128.codecInt.encode(624485).require
      val expected =
        BitVector(Array[Byte](0xe5.toByte, 0x8e.toByte, 0x26.toByte))
      encoded shouldBe expected

      val decoded = Leb128.codecInt.decodeValue(encoded).require
      decoded shouldBe 624485
    }
  }
