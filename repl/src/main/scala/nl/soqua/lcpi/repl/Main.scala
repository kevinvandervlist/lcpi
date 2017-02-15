package nl.soqua.lcpi.repl

import java.io.File

import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO}
import nl.soqua.lcpi.repl.monad.{ReplCompiler, ReplState}
import nl.soqua.lcpi.repl.parser.StdInParser
import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder

import scala.io.Source
import scala.util.Try

object Main extends App {
  var ctx = CombinatorLibrary loadIn Context()

  val prompt = "lcpi λ> "

  println(
    """
      |A λ-calculus interpreter. Type `help` for usage information.
    """.stripMargin
  )

  var state = ReplState.empty
  var effect: Any = _

  val terminal = TerminalBuilder.builder()
    .system(true)
    .build()

  val lineReader = LineReaderBuilder.builder()
    .terminal(terminal)
    .build()

  val compiler = new DiskIO() with ReplCompiler {
    override def readFile(path: String): Try[Stream[String]] = Try {
      Source.fromFile(new File(path)).getLines().toStream
    }
  }

  while (!state.terminated) {
    val line = Try(lineReader.readLine(prompt)).getOrElse("quit")
    StdInParser(line) match {
      case Left(e) => println(e)
      case Right(cmd) => state = cmd.foldMap(compiler.compile).run(state).value match {
        case (s: ReplState, effect: String) => println(effect); s
        case (s: ReplState, _) => s
      }
    }
  }
}
