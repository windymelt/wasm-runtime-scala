package dev.capslock.scodecexercise
package wasm
package exec

import scodec.bits.BitVector

class RuntimeTest extends UnitTest:
  describe("Runtime") {
    it("should do add") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func (param i32 i32) (result i32)
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
        val result = Runtime.call(runtime, 0, args)
        result shouldBe Some(expected)
      }
    }
  }
