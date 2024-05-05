package dev.capslock.scodecexercise.wasm
package exec

import scala.util.Try

type ImportFunc = (Store, Vector[Value]) => Try[Option[Value]]
type Import     = Map[String, Map[String, ImportFunc]]
