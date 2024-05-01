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
    val store = Store(wasmBinary)
    val stack = mutable.Stack.empty[Value]
    val callStack = mutable.Stack.empty[Frame]
    Runtime(store, stack, callStack)
  }

  def call(runtime: Runtime, idx: Int, args: Vector[Value]): Option[Value] = {
    val func = runtime.store.funcs(idx)
    println(s"calling func: $func")
    runtime.stack.pushAll(args)
    println(s"stack: ${runtime.stack}")
    invoke(runtime, func)
  }

  @tailrec
  def execute(runtime: Runtime): Unit = {
    println("executing frame")
    if (runtime.callStack.isEmpty) {
      return
    }

    val frame = runtime.callStack.pop()
    println(s"current frame: $frame")
    if (!frame.insts.isDefinedAt(frame.pc)) {
      println("frame is done")
      println(s"stack: ${runtime.stack}")
      return
    }
    val instruction = frame.insts(frame.pc)
    println(s"current instruction: $instruction")
    println(s"current stack: ${runtime.stack}")

    val (newStack, newFrame) = instruction match {
      case Instruction.End =>
        println("inst: END")
        val frame = runtime.callStack.pop()
        unwindStack(runtime.stack, frame.sp, frame.arity)
        (runtime.stack, runtime.callStack.head)

      case Instruction.LocalGet(index) =>
        println(s"inst: LOCAL_GET($index)")
        val value = frame.locals(index)
        runtime.stack.push(value)
        val newFrame = frame.copy(pc = frame.pc + 1)
        (runtime.stack, newFrame)

      case Instruction.I32Add =>
        println("inst: I32_ADD")
        val (a, b) = (
          runtime.stack.pop().asInstanceOf[Value.I32],
          runtime.stack.pop().asInstanceOf[Value.I32],
        )
        val result = Value.I32(a.value + b.value)
        runtime.stack.push(result)
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
  ) = {
    arity match
      case 0 =>
        for (_ <- 0 to sp) stack.pop()
      case _ =>
        val result = stack.pop()
        for (_ <- 0 to sp) stack.pop()
        stack.push(result)
  }

  private def invoke(runtime: Runtime, func: FuncInst): Option[Value] = {
    var locals = mutable.Stack.empty[Value]
    for (_ <- func.typ.params.indices) locals.push(runtime.stack.pop())
    println(s"locals: $locals")

    for (local <- func.code.locals) {
      local match {
        case ValueType.I32 =>
          locals = locals :+ Value.I32(0)
        case ValueType.I64 =>
          locals = locals :+ Value.I64(0)
      }
    }

    val arity = func.typ.results.size
    println(s"returning arity: $arity")
    val frame = Frame(
      pc = 0,
      sp = runtime.stack.size,
      insts = func.code.body,
      arity = arity,
      locals = locals.toVector,
    )
    println(s"pushing frame: $frame")

    runtime.callStack.push(frame)

    try {
      execute(runtime)
    } catch {
      case _: Throwable =>
        cleanup(runtime)
        return None
    }

    arity match {
      case 0 =>
        None
      case _ =>
        Some(runtime.stack.pop())
    }
  }

  private def cleanup(runtime: Runtime) = {
    runtime.callStack.clear()
    runtime.stack.clear()
  }
}
