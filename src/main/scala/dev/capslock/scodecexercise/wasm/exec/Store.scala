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

    val funcTypeIdxs = wasmBinary.sections
      .filter(_.header.sectionCode == SectionCode.FunctionSection)
      .flatMap(
        _.payload.asInstanceOf[FuncSection].functionTypeIndices,
      )

    val types = wasmBinary.sections
      .filter(_.header.sectionCode == SectionCode.TypeSection)
      .flatMap(_.payload.asInstanceOf[TypeSection].types)

    val funcs = wasmBinary.sections.view
      .filter(_.header.sectionCode == SectionCode.CodeSection)
      .flatMap(_.payload.asInstanceOf[CodeSection].functions)
      .zip(funcTypeIdxs)
      .map { case (code, funcTypeIdx) =>
        val funcType = types(funcTypeIdx)
        FuncInst(
          funcType,
          Func(code.locals.map(_._2), code.body),
        )
      }

    // Exports

    val exports = wasmBinary.sections.view
      .filter(_.header.sectionCode == SectionCode.ExportSection)
      .map(_.payload.asInstanceOf[ExportSection])
      .flatMap(sec =>
        sec.exports.map(ex => ex.name -> ExportInst(ex.name, ex.desc)),
      )
      .toMap

    Store(funcs.toVector, ModuleInst(exports))
  }
