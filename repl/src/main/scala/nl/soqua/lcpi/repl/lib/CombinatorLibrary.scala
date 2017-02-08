package nl.soqua.lcpi.repl.lib

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.parser.repl.ReplParser

object CombinatorLibrary {
  def loadIn(ctx: Context): Context = library.foldLeft(ctx)(parseAndStore)

  private val library = List(
    // Identity func
    "I := λx.x",
    // K (constant)
    "K := λx.λy.x",
    // S
    "S := λx.λy.λz.x z (y z)",
    // Church booleans
    "TRUE := λx.λy.x",
    "FALSE := λx.λy.y",
    // if-then-else
    "IF := λp.λi.λe.p i e",
    // Y combinator
    "Y := λf.(λx.f (x x)) (λx.f (x x))"
  )

  private def parseAndStore(ctx: Context, expr: String): Context = {
    ReplParser(expr) match {
      case Right(Assignment(v, e)) => ctx.assign(v, e)
      case _ => ctx
    }
  }
}
