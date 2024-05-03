package dev.capslock.scodecexercise.wasm
package exec

import dev.capslock.scodecexercise.wasm.types.ValueType
import function.Instruction

import scala.annotation.tailrec
import scala.collection.mutable

case class Runtime(
    store: Store,
    stack: mutable.Stack[Value],
    callStack: mutable.Stack[Frame],
)

object Runtime {
  def apply(wasmBinary: WasmBinary): Runtime = {
    val (stack, callStack) =
      (mutable.Stack.empty[Value], mutable.Stack.empty[Frame])
    Runtime(Store(wasmBinary), stack, callStack)
  }

  def call(
      runtime: Runtime,
      funcName: String,
      args: Vector[Value],
  ): Option[Value] = {
    for
      exported <- runtime.store.module.exports.get(funcName)
      funcIdx <- exported.desc match
        case dev.capslock.scodecexercise.wasm.types.ExportDesc.Func(idx) =>
          Some(idx)
      func = runtime.store.funcs(funcIdx)
      _ = runtime.stack.pushAll(args)
      result <- invoke(runtime, func)
    yield result
  }

  @tailrec
  def execute(runtime: Runtime): Unit = {
    if (runtime.callStack.isEmpty) return

    val frame = runtime.callStack.pop()

    if (!frame.insts.isDefinedAt(frame.pc)) return
    val instruction = frame.insts(frame.pc)

    val (newStack, newFrame) = instruction match {
      case Instruction.End =>
        val frame = runtime.callStack.pop()
        unwindStack(runtime.stack, frame.sp, frame.arity)
        (runtime.stack, runtime.callStack.head)

      case Instruction.LocalGet(index) =>
        runtime.stack.push(frame.locals(index))
        val newFrame = frame.copy(pc = frame.pc + 1)
        (runtime.stack, newFrame)

      case Instruction.I32Add =>
        val (a, b) = (
          runtime.stack.pop().asInstanceOf[Value.I32],
          runtime.stack.pop().asInstanceOf[Value.I32],
        )
        runtime.stack.push(Value.I32(a.value + b.value))
        val newFrame = frame.copy(pc = frame.pc + 1)
        (runtime.stack, newFrame)
    }

    runtime.callStack.push(newFrame)

    execute(runtime.copy(stack = newStack))
  }

  private def unwindStack(
      stack: mutable.Stack[Value],
      sp: Int,
      arity: Int,
  ) = arity match
    case 0 =>
      for (_ <- 0 to sp) stack.pop()
    case _ =>
      val result = stack.pop()
      for (_ <- 0 to sp) stack.pop()
      stack.push(result)

  private def invoke(runtime: Runtime, func: FuncInst): Option[Value] = {
    var locals = mutable.Stack.empty[Value]
    for (_ <- func.typ.params.indices) locals.push(runtime.stack.pop())

    for (local <- func.code.locals) {
      local match
        case ValueType.I32 =>
          locals = locals :+ Value.I32(0)
        case ValueType.I64 =>
          locals = locals :+ Value.I64(0)
    }

    val arity = func.typ.results.size
    val frame = Frame(
      pc = 0,
      sp = runtime.stack.size,
      insts = func.code.body,
      arity = arity,
      locals = locals.toVector,
    )

    runtime.callStack.push(frame)

    try {
      execute(runtime)
    } catch {
      case _: Throwable =>
        cleanup(runtime)
        return None
    }

    arity match
      case 0 => None
      case _ => Some(runtime.stack.pop())
  }

  private def cleanup(runtime: Runtime) =
    runtime.callStack.clear()
    runtime.stack.clear()
}
