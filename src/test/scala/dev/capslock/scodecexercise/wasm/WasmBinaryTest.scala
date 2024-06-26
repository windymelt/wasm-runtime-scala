package dev.capslock.scodecexercise.wasm

import scodec.*
import scodec.bits.*
import dev.capslock.scodecexercise.UnitTest
import dev.capslock.scodecexercise.wasm.SectionCode.FunctionSection
import dev.capslock.scodecexercise.wasm.sections.{
  CodeSection,
  ExportSection,
  FuncSection,
  TypeSection,
}
import dev.capslock.scodecexercise.wasm.types.ValueType.I32
import dev.capslock.scodecexercise.wasm.types.{FuncType, ValueType}
import function.Instruction
import dev.capslock.scodecexercise.wasm.types.{Export, ExportDesc}

class WasmBinaryTest extends UnitTest:
  describe("WasmBinary") {
    it("should parse wasm binary") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func
          |    (local i32)
          |    (local i64 i64)
          |  )
          |)
          |""".stripMargin)
      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val expected = WasmBinary(
        Preamble(1),
        Vector(
          Section(
            SectionHeader(SectionCode.TypeSection, 4),
            TypeSection(Vector(FuncType(Vector(), Vector()))),
          ),
          Section(SectionHeader(FunctionSection, 2), FuncSection(Vector(0))),
          Section(
            SectionHeader(SectionCode.CodeSection, 8),
            CodeSection(
              Vector(
                Function(
                  Vector((1, ValueType.I32), (2, ValueType.I64)),
                  Vector(Instruction.End),
                ),
              ),
            ),
          ),
        ),
      )

      result shouldBe expected
    }

    it("should parse instructions") {
      val wasmBinary = wat2wasm("""
          |(module
          |  (func (param i32 i32) (result i32)
          |    (local.get 0)
          |    (local.get 1)
          |    i32.add
          |  )
          |)
          |""".stripMargin)
      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      val expected = WasmBinary(
        Preamble(1),
        Vector(
          Section(
            SectionHeader(SectionCode.TypeSection, 7),
            TypeSection(
              Vector(
                FuncType(
                  Vector(ValueType.I32, ValueType.I32),
                  Vector(ValueType.I32),
                ),
              ),
            ),
          ),
          Section(SectionHeader(FunctionSection, 2), FuncSection(Vector(0))),
          Section(
            SectionHeader(SectionCode.CodeSection, 9),
            CodeSection(
              Vector(
                Function(
                  Vector(),
                  Vector(
                    Instruction.LocalGet(0),
                    Instruction.LocalGet(1),
                    Instruction.I32Add,
                    Instruction.End,
                  ),
                ),
              ),
            ),
          ),
        ),
      )

      result shouldBe expected
    }

    it("should parse exported function") {
      val wasmBinary = wat2wasm("""
        |(module
        |  (func (export "f") (result i32)
        |    (i32.const 65535)
        |  )
        |)
        |""".stripMargin)

      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require

      val expected = WasmBinary(
        Preamble(1),
        Vector(
          Section(
            SectionHeader(SectionCode.TypeSection, 5),
            TypeSection(
              Vector(
                FuncType(
                  Vector.empty,
                  Vector(ValueType.I32),
                ),
              ),
            ),
          ),
          Section(SectionHeader(FunctionSection, 2), FuncSection(Vector(0))),
          Section(
            SectionHeader(SectionCode.ExportSection, 5),
            ExportSection(Vector(Export("f", ExportDesc.Func(0)))),
          ),
          Section(
            SectionHeader(SectionCode.CodeSection, 8),
            CodeSection(
              Vector(
                Function(
                  Vector(),
                  Vector(
                    Instruction.I32Const(65535),
                    Instruction.End,
                  ),
                ),
              ),
            ),
          ),
        ),
      )

      result shouldBe expected
    }

    it("should parse call instruction") {
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

      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      println(result)
    }

    it("should parse i32.store") {
      val wasmBinary = wat2wasm("""
        | (module (memory 1) (func (i32.store (i32.const 0) (i32.const 42))))
        |""".stripMargin)

      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      println(result)
    }

    it("should parse import section") {
      val wasmBinary = wat2wasm("""
        | (module
        |  (import "env" "double" (func $double (param i32) (result i32)))
        |  (func (export "call_doubler") (param i32) (result i32)
        |    (local.get 0)
        |    (call $double)
        |  )
        |)
        |""".stripMargin)

      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      println(result)
    }

    it("should parse memory section") {
      val wasmBinary = wat2wasm("""
          |(module (memory 1 2))
          |""".stripMargin)

      val result = WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require
      println(result)
    }

    it("should parse data section") {
      val wasmBinary = wat2wasm("""
            |(module
            |  (memory 1)
            |  (data (i32.const 0) "hello, world")
            |)
            |""".stripMargin)

      WasmBinary.codec.decodeValue(BitVector(wasmBinary)).require

      val wasmBinary2 = wat2wasm("""
            |(module
            |  (memory 1)
            |  (data (i32.const 0) "hello")
            |  (data (i32.const 5) "world")
            |)
            |""".stripMargin)

      WasmBinary.codec.decodeValue(BitVector(wasmBinary2)).require
    }
  }
