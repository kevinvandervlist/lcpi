package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.Term

object Interpreter {
  def apply(ctx: Context, term: Term): Either[InterpreterError, Term] = Left(InterpreterError("abc"))
}
