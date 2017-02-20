package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.interpreter.ReplExpression._
import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.{Expression, Variable}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Interpreter {

  import transformation._

  private def existingVarError(v: Variable) = InterpreterError(s"The variable '${v.symbol}' has already been assigned in the context")

  private def inputAsExpressionWithImmutableContext(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] =
    expression match {
      case Assignment(v, _) if ctx.contains(v) => Left(existingVarError(v))
      case _ => inputAsExpressionWithMutableContext(ctx, expression)
    }

  private def inputAsExpressionWithMutableContext(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] =
    expression match {
      case Assignment(v, expr) => Right(InterpreterResult(ctx.assign(v, expr), re2e(expr)))
      case nl.soqua.lcpi.ast.interpreter.Expression(expr) => Right(InterpreterResult(ctx, expr))
    }

  def mutableApply(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] = for {
    expr <- inputAsExpressionWithMutableContext(ctx, expression)
    i <- Interpreter(expr.context, expr.expression)
  } yield i

  def apply(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] = for {
    expr <- inputAsExpressionWithImmutableContext(ctx, expression)
    i <- Interpreter(expr.context, expr.expression)
  } yield i

  def apply(ctx: Context, term: Expression): Either[InterpreterError, InterpreterResult] = {
    // First retrieve any variables that are stored in the context
    @tailrec def substituteContext(in: Expression): Expression = {
      ctx.foldLeft(in)((t, v, f) => substitute(t, v, f)) match {
        case result if result == in => result
        case result => substituteContext(result)
      }
    }

    // Then normalize them
    val normalized = List(α _, β _, η _).foldLeft(substituteContext(term))((t, f) => f(t))

    Right(InterpreterResult(ctx, normalized))
  }

  def trace(ctx: Context, expression: ReplExpression): Either[InterpreterError, InterpreterResult] = for {
    expr <- inputAsExpressionWithImmutableContext(ctx, expression)
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
    val out: mutable.ListBuffer[(String, Expression)] = ListBuffer.empty

    def substituteContext(in: Expression): List[(String, Expression)] = {
      @tailrec def f(in: Expression, res: List[(String, Expression)]): List[(String, Expression)] = {
        ctx.foldLeft(in)((t, v, f) => substitute(t, v, f)) match {
          case result if result == in => List("S" -> result)
          case result => f(result, ("S" -> result) :: res)
        }
      }
      f(in, List.empty)
    }

    val trace = "S" -> term :: substituteContext(term)
    out ++ trace -> trace.last._2
  }
}
