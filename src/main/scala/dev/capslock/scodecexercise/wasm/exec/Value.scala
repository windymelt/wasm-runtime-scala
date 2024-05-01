package dev.capslock.scodecexercise.wasm.exec

enum Value:
  case I32(value: Int)
  case I64(value: Long)
  case F32(value: Float)
  case F64(value: Double)
