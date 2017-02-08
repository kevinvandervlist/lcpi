package nl.soqua.lcpi.repl

import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.lib.CombinatorLibrary
import nl.soqua.lcpi.repl.monad.{ReplCompiler, ReplState}
import nl.soqua.lcpi.repl.parser.StdInParser

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

  for (ln <- io.Source.stdin.getLines) {
    StdInParser(ln) match {
      case Left(e) => println(e)
      case Right(cmd) => cmd.foldMap(ReplCompiler.pureCompiler).run(state).value match {
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
