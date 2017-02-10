package nl.soqua.lcpi.repl.lib

import scala.util.Try

trait DiskIO {
  def readFile(path: String): Try[Stream[String]]
}
