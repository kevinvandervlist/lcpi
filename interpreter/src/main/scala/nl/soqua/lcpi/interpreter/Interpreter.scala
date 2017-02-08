package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.interpreter.ReplExpression._
import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.{Expression, Variable}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Interpreter {

  import transformation._

  private def existingVarError(v: Variable) = InterpreterError(s"The variable '${v.symbol}' has already been assigned in the context")

  private def inputAsExpressionWithImmutableInput(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] =
    expression match {
      case Assignment(v, _) if ctx.contains(v) => Left(existingVarError(v))
      case Assignment(v, expr) => Right(InterpreterResult(ctx.assign(v, expr), re2e(expr)))
      case nl.soqua.lcpi.ast.interpreter.Expression(expr) => Right(InterpreterResult(ctx, expr))
    }

  def apply(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] = for {
    expr <- inputAsExpressionWithImmutableInput(ctx, expression)
    i <- Interpreter(expr.context, expr.expression)
  } yield i

  def apply(ctx: Context, term: Expression): Either[InterpreterError, InterpreterResult] = {
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

    Right(InterpreterResult(ctx, normalized))
  }

  def trace(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] = for {
    expr <- inputAsExpressionWithImmutableInput(ctx, expression)
    i <- Interpreter.trace(expr.context, expr.expression)
  } yield i

  def trace(ctx: Context, term: Expression): Either[InterpreterError, InterpreterResult] = {
    var (out, expression) = substituteFromContextDeclarations(ctx, term)

    expression = α(expression)
    out += (("α", expression))

    out ++= βtrace(expression).map(x => ("β", x))
    expression = out.last._2

    expression = η(expression)
    out += (("η", expression))

    Right(InterpreterResult(ctx, out.toList))
  }

  private def substituteFromContextDeclarations(ctx: Context, term: Expression): (mutable.ListBuffer[(String, Expression)], Expression) = {
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
