package dev.capslock.scodecexercise.wasm
package exec

import sections.{CodeSection, FuncSection, TypeSection}
import types.{FuncType, ValueType}
import function.Instruction

case class Store(funcs: Vector[FuncInst])

case class FuncInst(typ: FuncType, code: Func)

case class Func(locals: Vector[ValueType], body: Vector[Instruction])

object Store:
  def apply(wasmBinary: WasmBinary): Store = {
    val funcTypeIdxs = wasmBinary.sections.find(
      _.header.sectionCode == SectionCode.FunctionSection,
    ) match
      case Some(value) =>
        value.payload.asInstanceOf[FuncSection].functionTypeIndices
      case None => Vector.empty

    var funcs = collection.mutable.Seq.empty[FuncInst]
    val types = wasmBinary.sections.find(
      _.header.sectionCode == SectionCode.TypeSection,
    ) match
      case Some(value) => value.payload.asInstanceOf[TypeSection].types
      case None        => Vector.empty

    wasmBinary.sections.find(
      _.header.sectionCode == SectionCode.CodeSection,
    ) match
      case Some(value) =>
        val codeSection = value.payload.asInstanceOf[CodeSection]
        codeSection.functions.zip(funcTypeIdxs).foreach {
          case (code, funcTypeIdx) =>
            val funcType = types(funcTypeIdx)
            funcs = funcs :+ FuncInst(
              funcType,
              Func(code.locals.map(_._2), code.body),
            )
        }
      case None => ???

    Store(funcs.toVector)
  }
