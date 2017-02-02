package nl.soqua.lcpi.ast.lambda

object Expression {
  def V(symbol: String): Variable = Variable(symbol)

  def λ(symbol: String, body: Expression): LambdaAbstraction = λ(Variable(symbol), body)

  def λ(variable: Variable, body: Expression): LambdaAbstraction = LambdaAbstraction(variable, body)

  def A(t: Expression, s: Expression): Expression = Application(t, s)
}

sealed trait Expression

case class Variable(symbol: String) extends Expression

case class LambdaAbstraction(variable: Variable, body: Expression) extends Expression

case class Application(t: Expression, s: Expression) extends Expression
