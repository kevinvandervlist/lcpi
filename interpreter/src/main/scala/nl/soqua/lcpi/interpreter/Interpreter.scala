package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.repl.ReplParser

object Interpreter {

  import transformation._

  private val existingVarError = InterpreterError("The given variable has already been assigned in the context")

  private def inputAsExpressionWithImmutableInput(ctx: Context, line: String): Either[InterpreterError, Expression] =
    ReplParser(line)
      .right
      .flatMap({
        case Assignment(v, _) if ctx.contains(v) => Left(existingVarError)
        case Assignment(v, expr) => ctx.assign(v, expr.expression); Right(expr.expression)
        case nl.soqua.lcpi.ast.interpreter.Expression(expr) => Right(expr)
      }).left
      .map({
        case ParserError(msg) => InterpreterError(s"Parser error occurred: $msg")
        case e: InterpreterError => e
      })

  def apply(ctx: Context, line: String): Either[InterpreterError, Expression] = for {
    expr <- inputAsExpressionWithImmutableInput(ctx, line)
    i <- Interpreter(ctx, expr)
  } yield i

  def apply(ctx: Context, term: Expression): Either[InterpreterError, Expression] = {
    // First retrieve any variables that are stored in the context
    val substituted = ctx.foldLeft(term)((t, v, f) => substitute(t, v, f))
    // Then normalize them
    //val normalized = List(α _, β _, η _).foldLeft(substituted)((t, f) => f(t))
    val normalized = List(α _, β _, η _).foldRight(substituted)((f, t) => f(t))

    Right(normalized)
  }
}
