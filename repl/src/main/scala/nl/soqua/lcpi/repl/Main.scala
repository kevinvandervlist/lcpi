package nl.soqua.lcpi.repl

import java.io.File

import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO}
import nl.soqua.lcpi.repl.monad.{ReplCompiler, ReplState}
import nl.soqua.lcpi.repl.parser.StdInParser

import scala.io.Source
import scala.util.Try

object Main extends App {
  var ctx = CombinatorLibrary loadIn Context()

  def drawShell(): Unit = print("lcpi λ> ")

  println(
    """
      |A λ-calculus interpreter. Type `help` for usage information.
    """.stripMargin
  )

  drawShell()

  var state = ReplState.empty
  var effect: Any = _

  val compiler = ReplCompiler.compiler(new DiskIO() {
    override def load(path: String): Try[Stream[String]] = Try {
      Source.fromFile(new File(path)).getLines().toStream
    }
  })

  for (ln <- io.Source.stdin.getLines) {
    StdInParser(ln) match {
      case Left(e) => println(e)
      case Right(cmd) => cmd.foldMap(compiler).run(state).value match {
        case (s: ReplState, _) if s.terminated =>
          System.exit(0)
        case (s: ReplState, effect: String) =>
          state = s
          println(effect)
        case (s: ReplState, _) =>
          state = s
        case _ =>
      }
    }
    drawShell()
  }
}
