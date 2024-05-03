package dev.capslock.scodecexercise
package wasm
package exec

import scodec.bits.BitVector

class RuntimeTest extends UnitTest:
  describe("Runtime") {
    it("should call exported function add") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func (export "add") (param i32 i32) (result i32)
          |    (local.get 0)
          |    (local.get 1)
          |    i32.add
          |  )
          |)
          |""".stripMargin)
      val wasm = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val cases = Seq(
        (Vector(Value.I32(1), Value.I32(2)), Value.I32(3)),
        (Vector(Value.I32(3), Value.I32(4)), Value.I32(7)),
        (Vector(Value.I32(5), Value.I32(6)), Value.I32(11)),
        (Vector(Value.I32(10), Value.I32(5)), Value.I32(15)),
      )

      cases.foreach { case (args, expected) =>
        val result = Runtime.call(runtime, "add", args)
        result shouldBe Some(expected)
      }
    }

    it("should be able to call i32.const") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func (export "satanist") (result i32)
          |    i32.const 666
          |  )
          |)
          |""".stripMargin)
      val wasm = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "satanist", Vector.empty)
      result shouldBe Some(Value.I32(666))
    }

    it("should behave safely when calling non-existent function") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func (export "satanist") (result i32)
          |    i32.const 666
          |  )
          |)
          |""".stripMargin)
      val wasm = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "supercalifragilisticexpialidocious", Vector.empty)
      result shouldBe None
    }
  }
