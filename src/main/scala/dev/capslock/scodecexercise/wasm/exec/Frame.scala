package dev.capslock.scodecexercise.wasm
package exec

import function.Instruction

/** Frame represents a function call frame.
  * @param pc
  *   program counter
  * @param sp
  *   stack pointer
  * @param insts
  *   instructions
  * @param arity
  *   arity of the function
  * @param locals
  *   local variables
  */
case class Frame(
    pc: Int,
    sp: Int,
    insts: Vector[Instruction],
    arity: Int,
    locals: Vector[Value],
)
