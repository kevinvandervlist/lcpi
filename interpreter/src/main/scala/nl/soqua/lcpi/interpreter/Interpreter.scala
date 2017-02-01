package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.Expression

object Interpreter {
  def apply(ctx: Context, term: Expression): Either[InterpreterError, Expression] = Left(InterpreterError("abc"))
}
