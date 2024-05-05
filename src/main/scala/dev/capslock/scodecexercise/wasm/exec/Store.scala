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

    // Keeping the order of section because function index is based on the order of function section
    val funcs = wasmBinary.sections.view.zipWithIndex
      .filter(_._1.header.sectionCode == SectionCode.CodeSection)
      .flatMap((f, i) =>
        f.payload.asInstanceOf[CodeSection].functions.map(_ -> i),
      )
      .zip(funcTypeIdxs)
      .map { case ((code, i), funcTypeIdx) =>
        val funcType = types(funcTypeIdx)
        FuncInst.InternalFuncInst(
          funcType,
          Func(code.locals.map(_._2), code.body),
        ) -> i
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

    // Keeping the order of section because function index is based on the order of function section
    val imports = wasmBinary.sections.view.zipWithIndex
      .filter(_._1.header.sectionCode == SectionCode.ImportSection)
      .map((f, i) => f.payload.asInstanceOf[ImportSection] -> i)
      .flatMap((f, i) => f.imports.map(_ -> i))
      .map((imp, i) => (imp.module, ImportInst(imp.field, imp.desc), i))

    val importMap = imports.map((k, v, i) => k -> v).toMap

    val externalFuncs = imports.collect {
      case (
            module,
            ImportInst(name, ImportDesc.Func(funcTypeIdx)),
            orderOfSection,
          ) =>
        FuncInst.ExternalFuncInst(
          module,
          name,
          types(funcTypeIdx),
        ) -> orderOfSection
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

    val sortedFuncs = (funcs ++ externalFuncs).toSeq.sortBy(_._2).map(_._1)

    Store(
      funcs = sortedFuncs.toVector,
      module = ModuleInst(exports, importMap),
      memories = memories,
    )
  }
