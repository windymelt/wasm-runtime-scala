package dev.capslock.scodecexercise.wasm
package wasi

import exec.Store
import exec.Value
import scodec.bits.{BitVector, ByteVector}

import java.io.File
import scala.util.Try
import scodec.codecs.{int32L, bytes}

object WasiSnapshotPreview1 {
  val fileTable = Seq(
    java.io.FileDescriptor.in,
    java.io.FileDescriptor.out,
    java.io.FileDescriptor.err,
  )

  def invoke(
      store: Store,
      funcName: String,
      args: Vector[Value],
  ): Try[Option[Value]] = {
    funcName match
      case "fd_write" => fdWrite(store, args)
  }

  private def fdWrite(
      store: Store,
      values: Vector[Value],
  ): Try[Option[Value]] = {
    val args: Vector[Int] = values.map {
      case Value.I32(value) => value
      case _                => return Try(None)
    }

    var iovs = args(1)
    val (fd, _, iovsLen, rp) = args match {
      case Vector(fd, _, iovsLen, rp) => (fd, 0, iovsLen, rp)
      case _                          => return Try(None)
    }

    val file   = fileTable(fd)     // TODO: return error if file is not found
    val memory = store.memories(0) // TODO: return error if memory is not found

    var nWritten      = 0
    val MEM_ALIGNMENT = 4
    for _ <- 0 until iovsLen
    do {
      val start = memRead(memory.data, iovs).get
      iovs += MEM_ALIGNMENT

      val len = memRead(memory.data, iovs).get
      iovs += MEM_ALIGNMENT

      val end = start + len

      val dataSlice = memory.data.slice(start, end)

      val fos = new java.io.FileOutputStream(file)
      fos.write(dataSlice)
      nWritten = nWritten + dataSlice.length
    }

    memWrite(memory.data, rp, int32L.encode(nWritten).require.toByteArray).get
    Try(Some(Value.I32(0)))
  }

  private def memRead(buf: Array[Byte], start: Int): Try[Int] =
    int32L.decodeValue(BitVector(buf.drop(start))).toTry

  private def memWrite(
      buf: Array[Byte],
      start: Int,
      data: Array[Byte],
  ): Try[Unit] =
    bytes.encode(ByteVector(data)).toTry.map { bits =>
      bits.toByteArray.copyToArray(buf, start)
    }
}
