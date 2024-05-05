package dev.capslock.scodecexercise

package object wasm:
  def wat2wasm(wat: String): Array[Byte] = {
    val watFile = os.temp(wat, prefix = "wat2wasm", deleteOnExit = true)
    val process = os
      .proc("/usr/local/bin/wat2wasm", "--output=-", watFile.toString)
      .spawn()
    process.stdout.readAllBytes()
  }
