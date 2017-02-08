package nl.soqua.lcpi.repl.lib

import scala.util.Try

trait DiskIO {
  def load(path: String): Try[Stream[String]]
}
