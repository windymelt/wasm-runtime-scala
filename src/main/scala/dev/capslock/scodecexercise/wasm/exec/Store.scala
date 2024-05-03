package dev.capslock.scodecexercise.wasm
package exec

import sections.{CodeSection, FuncSection, TypeSection, ExportSection}
import types.{FuncType, ValueType}
import function.Instruction
import types.ExportDesc

case class Store(
    funcs: Vector[FuncInst],
    module: ModuleInst,
)

// Func

case class FuncInst(typ: FuncType, code: Func)

case class Func(locals: Vector[ValueType], body: Vector[Instruction])

// Module

case class ModuleInst(exports: Map[String, ExportInst])
case class ExportInst(name: String, desc: ExportDesc)

object Store:
  def apply(wasmBinary: WasmBinary): Store = {
    // Funcs

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

    // Exports

    val exports = wasmBinary.sections
      .filter(_.header.sectionCode == SectionCode.ExportSection)
      .map(_.payload.asInstanceOf[ExportSection])
      .flatMap(sec =>
        sec.exports.map(ex => ex.name -> ExportInst(ex.name, ex.desc)),
      )
      .toMap

    Store(funcs.toVector, ModuleInst(exports))
  }
