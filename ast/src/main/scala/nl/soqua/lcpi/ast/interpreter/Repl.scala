package nl.soqua.lcpi.ast.interpreter

import nl.soqua.lcpi.ast.lambda.Variable

import scala.language.implicitConversions

sealed trait ReplExpression

object ReplExpression {
  // use these implicit conversions for convenience, since the expression types are isomorphic
  implicit def re2e(e: Expression): nl.soqua.lcpi.ast.lambda.Expression = e.e

  implicit def e2re(e: nl.soqua.lcpi.ast.lambda.Expression): Expression = Expression(e)
}

case class Assignment(assignee: Variable, assignment: Expression) extends ReplExpression

case class Expression(e: nl.soqua.lcpi.ast.lambda.Expression) extends ReplExpression
