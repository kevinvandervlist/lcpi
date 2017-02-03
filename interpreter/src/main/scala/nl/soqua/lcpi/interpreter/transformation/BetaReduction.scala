package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

object BetaReduction {

  import Substitution.substitute

  /**
    * Beta reduction.
    *
    * @param e
    * @return
    */
  def β(e: Expression): Expression = {
    val x = βReduction(e)
    if (x == e) {
      x
    } else {
      β(x)
    }
  }

  /**
    * Alias for `β`
    *
    * @param e The expression to rewrite
    * @return A rewritten expression
    */
  def beta(e: Expression): Expression = β(e)

  private def βReduction(e: Expression): Expression = e match {
    case v: Variable => v
    case LambdaAbstraction(x, t) => LambdaAbstraction(x, βReduction(t))
    case Application(LambdaAbstraction(name, body), t) => substitute(body, name, t)
    case Application(s, t) => Application(βReduction(s), βReduction(t))
  }
}
