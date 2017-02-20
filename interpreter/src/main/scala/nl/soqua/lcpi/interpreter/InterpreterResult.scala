package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression

object InterpreterResult {
  def apply(ctx: Context, expression: Expression): InterpreterResult = SingleExpressionInterpreterResult(ctx, expression)

  def apply(ctx: Context, trace: List[(String, Expression)]): InterpreterResult = TraceInterpreterResult(ctx, trace.last._2, trace)
}

sealed trait InterpreterResult {
  val expression: Expression
  val context: Context
  def map(f: Expression => Expression): InterpreterResult
}

case class SingleExpressionInterpreterResult(context: Context, expression: Expression) extends InterpreterResult {
  override def map(f: (Expression) => Expression): InterpreterResult = copy(expression = f(expression))
}

case class TraceInterpreterResult(context: Context, expression: Expression, trace: List[(String, Expression)]) extends InterpreterResult {
  override def map(f: (Expression) => Expression): InterpreterResult = copy(expression = f(expression))
}