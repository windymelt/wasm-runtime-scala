package dev.capslock.scodecexercise

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MySuite extends UnitTest:
  describe("scodec-bits") {
    it("literal for ByteVector") {
      import scodec.bits._
      val obtained = hex"deadbeef"
      val expected = ByteVector(0xde, 0xad, 0xbe, 0xef)
      obtained shouldBe expected
    }
    it("literal for BitVector") {
      import scodec.bits._
      val obtained = bin"11010"
      val expected = hex"d0".bits.take(5)
      obtained shouldBe expected
    }
    it("construct ByteVector from Array[Byte]") {
      import scodec.bits._
      val obtained = ByteVector(
        Array[Byte](0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte),
      )
      val expected = ByteVector(0xde, 0xad, 0xbe, 0xef)
      obtained shouldBe expected
    }
    it("transform ByteVector to BitVector") {
      import scodec.bits._
      val bv = hex"deadbeef".bits
      val expected = BitVector(0xde, 0xad, 0xbe, 0xef)
      bv shouldBe expected
    }
    it("transform BitVector to ByteVector") {
      import scodec.bits._
      val bv = BitVector(0xde, 0xad, 0xbe, 0xef)
      val expected = hex"deadbeef"
      bv.bytes shouldBe expected
    }
    it("setting specific bits") {
      import scodec.bits._
      val bv = bin"0000"
      val obtained = bv.set(1)
      val expected = bin"0100"
      obtained shouldBe expected

      val recovered = obtained.clear(1)
      recovered shouldBe bv

      val obtained2 = bv.update(1, true)
      obtained2 shouldBe obtained
    }
  }
  describe("scodec-core") {
    import scodec.*
    import scodec.bits.*
    import scodec.codecs.*

    it("use predefined codecs") {
      val codec = uint8
      val encoded = codec.encode(42)
      val obtained = encoded.require
      val expected = hex"2a".bits
      obtained shouldBe expected

      val decoded = codec.decode(obtained)
      decoded.require.value shouldBe 42
    }
    it("build complex codecs") {
      val codec = (uint8 :: uint8).as[(Int, Int)]
      val encoded = codec.encode((42, 43))
      val obtained = encoded.require
      val expected = hex"2a2b".bits
      obtained shouldBe expected

      val decoded = codec.decode(obtained)
      decoded.require.value shouldBe (42, 43)
    }
    it("map to case class") {
      case class Foo(a: Int, b: Int)
      val codec = (uint8 :: uint8).as[Foo]
      val encoded = codec.encode(Foo(42, 43))
      val obtained = encoded.require
      val expected = hex"2a2b".bits
      obtained shouldBe expected

      val decoded = codec.decode(obtained)
      decoded.require.value shouldBe Foo(42, 43)
    }
    it("treat with magic number") {
      val magic = hex"deadbeef"
      val codec = (constant(magic) :: uint8 :: uint8).as[(Int, Int)]
      val encoded = codec.encode((42, 43))
      val obtained = encoded.require
      val expected = hex"deadbeef2a2b".bits
      obtained shouldBe expected

      val decoded = codec.decode(obtained)
      decoded.require.value shouldBe (42, 43)
    }
    it("discriminator") {
      enum Shape derives Codec {
        case Circle(radius: Short)
        case Rectangle(width: Short, height: Short)
      }
      val cs = summon[Codec[Shape]]

      val codec = discriminated[Shape]
        .by(uint8)
        .typecase(0, cs)
        .typecase(1, cs)
      val encoded = codec.encode(Shape.Circle(42))
      val obtained = encoded.require
      val expected = hex"0000 002a".bits
      obtained shouldBe expected

      val decoded = codec.decode(obtained)
      decoded.require.value shouldBe Shape.Circle(42)

      val encoded2 = codec.encode(Shape.Rectangle(42, 43))
      val obtained2 = encoded2.require
      val expected2 = hex"0001 002a 002b".bits
      obtained2 shouldBe expected2

      val decoded2 = codec.decode(obtained2)
      decoded2.require.value shouldBe Shape.Rectangle(42, 43)
    }
  }
end MySuite
