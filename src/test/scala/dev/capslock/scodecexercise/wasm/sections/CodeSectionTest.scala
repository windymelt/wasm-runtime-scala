package dev.capslock.scodecexercise.wasm
package sections

import scodec.bits.*
import dev.capslock.scodecexercise.UnitTest
import dev.capslock.scodecexercise.wasm.function.Instruction

class CodeSectionTest extends UnitTest:
  describe("CodeSection") {
    it("should decode") {
      val bytes =
        hex"01 02 00 0b".bits // code section with 1 function, body size 2, local variables 0, end (0b)
      val codeSection = CodeSection.codecWithSize(4).decodeValue(bytes).require

      codeSection shouldBe CodeSection(
        Vector(Function(Vector(), Vector(Instruction.End))),
      )
    }
  }
