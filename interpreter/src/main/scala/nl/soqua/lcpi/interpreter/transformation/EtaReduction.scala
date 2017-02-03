package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

object EtaReduction {
  /**
    * Perform η-reduction on an expression
    * @param e The λ expression to reduce
    * @return A reduced expression
    */
  def η(e: Expression): Expression = e match {
    case v: Variable => v
    case LambdaAbstraction(x, t) => LambdaAbstraction(x, η(t))
    case Application(LambdaAbstraction(n1, Application(expr, n2)), arg) if n1 == n2 => Application(η(expr), η(arg))
    case Application(s, t) => Application(η(s), η(t))
  }

  /**
    * Alias for `η`
    */
  def eta(e: Expression): Expression = η(e)
}
