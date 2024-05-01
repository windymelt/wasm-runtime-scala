package dev.capslock.scodecexercise.wasm
package exec

import function.Instruction

case class Frame (pc: Int, sp: Int, insts: Vector[Instruction], arity: Int, locals: Vector[Value])
