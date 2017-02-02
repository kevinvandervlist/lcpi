package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.lambda.LambdaCalcParser

object Interpreter {

  import Transformation.{α, β, η}

  def apply(ctx: Context, expression: String): Either[InterpreterError, Expression] = {
    val e = for {
      p <- LambdaCalcParser(expression)
      i <- Interpreter(ctx, p)
    } yield i
    e.left.map({
      case ParserError(x, y) => InterpreterError(s"Parser error occurred: $x", y)
    })
  }

  def apply(ctx: Context, term: Expression): Either[InterpreterError, Expression] = {
    val normalized = List(α _, β _, η _).foldLeft(term)((t, f) => f(t))
    Right(normalized)
  }
}
