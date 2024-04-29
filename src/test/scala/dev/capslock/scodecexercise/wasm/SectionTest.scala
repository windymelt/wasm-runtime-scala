package dev.capslock.scodecexercise
package wasm

import dev.capslock.scodecexercise.wasm.function.Instruction
import dev.capslock.scodecexercise.wasm.sections.*
import dev.capslock.scodecexercise.wasm.types.FuncType
import scodec.bits.*

class SectionTest extends UnitTest:
  describe("section") {
    it("should decode type section") {
      val section =
        hex"01 04 01 60 00 00".bits // code 01, size 04, ...

      val result = Section.codec.decodeValue(section)
      println(result)

      result.require shouldBe Section(
        SectionHeader(SectionCode.TypeSection, 4),
        TypeSection(
          Vector(
            FuncType(Vector(), Vector()),
          ),
        ),
      )
    }

    it("should decode func section") {
      val section =
        hex"03 02 01 00".bits // code 03, size 02, count 01, function type idx 00

      val result = Section.codec.decodeValue(section)

      result.require shouldBe Section(
        SectionHeader(SectionCode.FunctionSection, 2),
        FuncSection(
          Vector(0),
        ),
      )
    }

    it("should decode code section") {
      // code 0a, size 04, function count 1, body size 02, local variables 00, instruments: 0b (end)
      val section =
        hex"""
             0a 04 01
             02 00
             0b
           """.bits

      val result = Section.codec.decodeValue(section)

      val expected = Section(
        SectionHeader(SectionCode.CodeSection, 4),
        CodeSection(
          Vector(
            Function(
              Vector(),
              Vector(
                Instruction.End,
              ),
            ),
          ),
        ),
      )

      result.require shouldBe expected
    }

    it("should decode multiple sections") {
      val section =
        hex"01 04 01 60 00 00 01 04 01 60 00 00".bits // code 01, size 04, ...

      val result = Section.codecAll.decodeValue(section)

      result.require shouldBe Vector(
        Section(
          SectionHeader(SectionCode.TypeSection, 4),
          TypeSection(
            Vector(
              FuncType(Vector(), Vector()),
            ),
          ),
        ),
        Section(
          SectionHeader(SectionCode.TypeSection, 4),
          TypeSection(
            Vector(
              FuncType(Vector(), Vector()),
            ),
          ),
        ),
      )
    }
  }
