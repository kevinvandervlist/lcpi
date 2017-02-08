package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.ast.interpreter.ReplExpression._
import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.repl.ReplParser

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Interpreter {

  import transformation._

  private val existingVarError = InterpreterError("The given variable has already been assigned in the context")

  private def inputAsExpressionWithImmutableInput(ctx: Context, line: String): Either[InterpreterError, Expression] =
    ReplParser(line)
      .right
      .flatMap({
        case Assignment(v, _) if ctx.contains(v) => Left(existingVarError)
        case Assignment(v, expr) => ctx.assign(v, expr); Right(re2e(expr))
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
    var changing = true
    var substituted = term
    do {
      val x = ctx.foldLeft(substituted)((t, v, f) => substitute(t, v, f))
      if (x == substituted) {
        changing = false
      } else {
        substituted = x
      }
    } while (changing)

    // Then normalize them
    val normalized = List(α _, β _, η _).foldLeft(substituted)((t, f) => f(t))

    Right(normalized)
  }

  def trace(ctx: Context, line: String): Either[InterpreterError, List[(String, Expression)]] = for {
    expr <- inputAsExpressionWithImmutableInput(ctx, line)
    i <- Interpreter.trace(ctx, expr)
  } yield i

  def trace(ctx: Context, term: Expression): Either[InterpreterError, List[(String, Expression)]] = {
    var (out, expression) = substituteFromContext(ctx, term)

    expression = α(expression)
    out += (("α", expression))

    out ++= βtrace(expression).map(x => ("β", x))
    expression = out.last._2

    expression = η(expression)
    out += (("η", expression))

    Right(out.toList)
  }

  private def substituteFromContext(ctx: Context, term: Expression): (mutable.ListBuffer[(String, Expression)], Expression) = {
    var out: mutable.ListBuffer[(String, Expression)] = ListBuffer.empty

    var changing = true
    var expression = term
    do {
      val initial = expression
      ctx.foreach((v, e) => {
        expression = substitute(expression, v, e)
        val entry = ("S", expression)
        if (!out.contains(entry)) {
          out += entry
        }
      })
      if (initial == expression) {
        changing = false
      }
    } while (changing)

    (out, expression)
  }
}
