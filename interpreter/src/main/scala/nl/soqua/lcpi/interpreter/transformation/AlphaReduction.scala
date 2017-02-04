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
  def α(e: Expression): Expression = α(e, List.empty)._1

  /**
    * Alias for `α`
    *
    * @param e The expression to rewrite
    * @return A rewritten expression
    */
  def alpha(e: Expression): Expression = α(e)

  /**
    * depth-first alpha-reduction that keeps track of encountered variables.
    */
  private def α(e: Expression, encountered: List[Variable]): (Expression, List[Variable]) = e match {
    case v: Variable => (v, encountered)
    case Application(t, s) =>
      val (t1, enc1) = α(t, encountered)
      val (s1, enc2) = α(s, enc1)
      (Application(t1, s1), enc2)
    case LambdaAbstraction(x, a) if encountered contains x =>
      val replacement = unused(x, encountered)
      α(LambdaAbstraction(replacement, substitute(a, x, replacement)), encountered)
    case LambdaAbstraction(x, a) => {
      val (body, enc1) = α(a, x :: encountered)
      (LambdaAbstraction(x, body), enc1)
    }
  }
}
