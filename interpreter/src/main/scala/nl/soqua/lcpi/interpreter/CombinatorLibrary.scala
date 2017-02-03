package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.parser.repl.ReplParser

object CombinatorLibrary {
  def loadIn(ctx: Context): Context = library.foldLeft(ctx)(parseAndStore)

  private val library = List(
    "I := λx.x",
    "T := λx.λy.x",
    "F := λx.λy.y",
    "Y := λf.(λx.(f(x x)) λx.(f(x x)))"
  )

  private def parseAndStore(ctx: Context, expr: String): Context = {
    ReplParser(expr) match {
      case Right(Assignment(v, e)) => ctx.assign(v, e)
      case _ =>
    }
    ctx
  }
}
