package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression

object InterpreterResult {
  def apply(ctx: Context, expression: Expression): InterpreterResult = SingleExpressionInterpreterResult(ctx, expression)

  def apply(ctx: Context, trace: List[(String, Expression)]): InterpreterResult = TraceInterpreterResult(ctx, trace.last._2, trace)
}

sealed trait InterpreterResult {
  val expression: Expression
  val context: Context
}

case class SingleExpressionInterpreterResult(context: Context, expression: Expression) extends InterpreterResult

case class TraceInterpreterResult(context: Context, expression: Expression, trace: List[(String, Expression)]) extends InterpreterResult