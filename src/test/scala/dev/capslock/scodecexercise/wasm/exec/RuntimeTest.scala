package dev.capslock.scodecexercise
package wasm
package exec

import dev.capslock.scodecexercise.wasm.exec.Value.I32
import scodec.bits.BitVector

import scala.util.Try

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
      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
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
      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "satanist", Vector.empty)
      result shouldBe Some(Value.I32(666))
    }

    it("should be able to call local.set") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func $local_set (result i32)
          |    (local $x i32)
          |    (local.set $x (i32.const 42))
          |    (local.get 0)
          |  )
          |  (export "local_set" (func $local_set))
          |)
          |""".stripMargin)
      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "local_set", Vector.empty)
      result shouldBe Some(Value.I32(42))
    }

    it("should be able to call CALL") {
      val wasmBinary = wat2wasm("""
      | (module
      |  (func (export "call_doubler") (param i32) (result i32) 
      |    (local.get 0)
      |    (call $double)
      |  )
      |  (func $double (param i32) (result i32)
      |    (local.get 0)
      |    (local.get 0)
      |    i32.add
      |  )
      |)
      |""".stripMargin)

      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "call_doubler", Vector(Value.I32(42)))
      result shouldBe Some(Value.I32(84))
    }

    it("should behave safely when calling non-existent function") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func (export "satanist") (result i32)
          |    i32.const 666
          |  )
          |)
          |""".stripMargin)
      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(
        runtime,
        "supercalifragilisticexpialidocious",
        Vector.empty,
      )
      result shouldBe None
    }

    it("can call external function") {
      val wasmBinary = wat2wasm("""
            |(module
            |  (import "env" "add" (func $add (param i32 i32) (result i32)))
            |  (func (export "call_add") (param i32 i32) (result i32)
            |    (local.get 0)
            |    (local.get 1)
            |    (call $add)
            |  )
            |)
            |""".stripMargin)
      val wasm = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)
        .withImport(
          "env",
          "add",
          (st: Store, args: Vector[Value]) =>
            println("here is external function")
            Try(
              Some(
                Value.I32(
                  args(0).asInstanceOf[Value.I32].value + args(1)
                    .asInstanceOf[Value.I32]
                    .value,
                ),
              ),
            ),
        )
        .get

      val result =
        Runtime.call(runtime, "call_add", Vector(Value.I32(42), Value.I32(42)))
      result shouldBe Some(Value.I32(84))
    }

    it("can safely exit when call non-existing external function") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (import "env" "supercalifragilisticexpialidocious" (func $supercalifragilisticexpialidocious (param i32 i32)))
          |  (func (export "call_add") (param i32 i32)
          |    (local.get 0)
          |    (local.get 1)
          |    (call $supercalifragilisticexpialidocious)
          |  )
          |)
          |""".stripMargin)
      val wasm = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)
        .withImport(
          "env",
          "add",
          (st: Store, args: Vector[Value]) =>
            println("here is external function")
            Try(
              Some(
                Value.I32(
                  args(0).asInstanceOf[Value.I32].value + args(1)
                    .asInstanceOf[Value.I32]
                    .value,
                ),
              ),
            ),
        )
        .get

      val result =
        Runtime.call(runtime, "call_add", Vector(Value.I32(42), Value.I32(42)))
      result shouldBe None
    }

    it("can store into memory") {
      import scodec.codecs.int32

      val wasmBinary = wat2wasm("""
            |(module
            |  (memory 1)
            |  (func (export "store") (param i32)
            |    (i32.store (i32.const 0) (local.get 0))
            |  )
            |)
            |""".stripMargin)
      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "store", Vector(Value.I32(42)))

      int32
        .decodeValue(BitVector(runtime.store.memories.head.data.take(4)))
        .require shouldBe 42
    }

    it("can call fd_write via WASI") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (import "wasi_snapshot_preview1" "fd_write"
          |    (func $fd_write (param i32 i32 i32 i32) (result i32))
          |  )
          |  (memory 1)
          |  (data (i32.const 0) "Hello, World via WASI!\n")
          |
          |  (func $hello_world (result i32)
          |    (local $iovs i32)
          |
          |    (i32.store (i32.const 24) (i32.const 0))
          |    (i32.store (i32.const 28) (i32.const 23))
          |
          |    (local.set $iovs (i32.const 24))
          |
          |    (call $fd_write
          |      (i32.const 1)
          |      (local.get $iovs)
          |      (i32.const 1)
          |      (i32.const 32)
          |    )
          |  )
          |  (export "_start" (func $hello_world))
          |)
          |""".stripMargin)

      val wasm    = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val runtime = Runtime(wasm)

      val result = Runtime.call(runtime, "_start", Vector.empty)

      result shouldBe Some(Value.I32(0))
    }
  }
