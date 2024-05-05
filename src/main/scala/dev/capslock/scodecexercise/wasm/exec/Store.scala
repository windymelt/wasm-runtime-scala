package dev.capslock.scodecexercise.wasm
package exec

import sections.{
  CodeSection,
  ExportSection,
  FuncSection,
  ImportSection,
  MemorySection,
  TypeSection,
  DataSection,
}
import types.{FuncType, ValueType}
import function.Instruction
import types.ExportDesc
import types.ImportDesc

val MEMORY_PAGE_SIZE = 65536 // 64 Ki. specified in WebAssembly spec

case class Store(
    funcs: Vector[FuncInst],
    module: ModuleInst,
    memories: Vector[MemoryInst],
)

// Func

enum FuncInst:
  case InternalFuncInst(typ: FuncType, code: Func)
  case ExternalFuncInst(module: String, func: String, funcType: FuncType)

case class Func(locals: Vector[ValueType], body: Vector[Instruction])

// Module

case class ModuleInst(
    exports: Map[String, ExportInst],
    imports: Map[String, ImportInst],
)
case class ExportInst(name: String, desc: ExportDesc)
case class ImportInst(name: String, desc: ImportDesc)

case class MemoryInst(data: Array[Byte], max: Option[Int])

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
        FuncInst.InternalFuncInst(
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

    // Imports

    val imports = wasmBinary.sections.view
      .filter(_.header.sectionCode == SectionCode.ImportSection)
      .map(_.payload.asInstanceOf[ImportSection])
      .flatMap(_.imports)
      .map(imp => imp.module -> ImportInst(imp.field, imp.desc))
      .toMap

    val externalFuncs = imports.view.collect {
      case (module, ImportInst(name, ImportDesc.Func(funcTypeIdx))) =>
        FuncInst.ExternalFuncInst(module, name, types(funcTypeIdx))
    }

    // Memories

    // Allocate memory with initial size
    val memories = wasmBinary.sections.view
      .filter(_.header.sectionCode == SectionCode.MemorySection)
      .flatMap(_.payload.asInstanceOf[MemorySection].mem)
      .map { memory =>
        val initialSize = memory.limits.min * MEMORY_PAGE_SIZE
        MemoryInst(Array.fill(initialSize)(0), memory.limits.max)
      }
      .toVector

    // Place data into memory
    wasmBinary.sections.view
      .filter(_.header.sectionCode == SectionCode.DataSection)
      .flatMap(_.payload.asInstanceOf[DataSection].data)
      .foreach { data =>
        val offset = data.offset
        val memory = memories(data.memoryIndex)
        data.init.copyToArray(memory.data, offset)
      }

    Store(
      funcs = (funcs ++ externalFuncs).toVector,
      module = ModuleInst(exports, imports),
      memories = memories,
    )
  }
