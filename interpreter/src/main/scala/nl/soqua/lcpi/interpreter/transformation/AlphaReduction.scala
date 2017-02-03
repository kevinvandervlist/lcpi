package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}
import nl.soqua.lcpi.interpreter.transformation.Names.unused

object AlphaReduction {

  import Substitution.substitute

  /**
    * Alpha reduction. Rewrite expressions so that every variable is unique.
    *
    * @param e The expression to rewrite
    * @return A rewritten expression.
    */
  def α(e: Expression): Expression = α(e, List.empty)

  /**
    * Alias for `α`
    *
    * @param e The expression to rewrite
    * @return A rewritten expression
    */
  def alpha(e: Expression): Expression = α(e)

  private def α(e: Expression, encountered: List[Variable]): Expression = e match {
    case v: Variable => v
    case Application(t, s) => Application(α(t, encountered), α(s, encountered))
    case LambdaAbstraction(x, a) if encountered contains x =>
      val replacement = unused(x, encountered)
      α(LambdaAbstraction(replacement, substitute(a, x, replacement)), encountered)
    case LambdaAbstraction(x, a) => LambdaAbstraction(x, α(a, x :: encountered))
  }
}
