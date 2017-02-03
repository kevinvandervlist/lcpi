package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

import scala.language.implicitConversions

object Stringify {
  implicit def expression2String(e: Expression): String = Stringify(e)

  def apply(e: Expression): String = e match {
    case v: Variable => v.symbol
    case Application(t, s) => s"(${apply(t)} ${apply(s)})"
    case LambdaAbstraction(x, a) => s"(λ${apply(x)}.${apply(a)})"
  }
}
