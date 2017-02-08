package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

object BetaReduction {

  import Substitution.substitute

  private[transformation] val maxDepth = 250 // a rather arbitrary max depth for β-reduction

  def βtrace(e: Expression): List[Expression] = βtrace(e: Expression, maxDepth)

  private def βtrace(e: Expression, depth: Int): List[Expression] = βReduction(e) match {
    case _ if depth == 0 => List.empty
    case x if x == e => List.empty
    case x => x :: βtrace(x, depth - 1)
  }

  /**
    * Beta reduction.
    *
    * @param e
    * @return
    */
  def β(e: Expression): Expression = β(e, maxDepth)

  private def β(e: Expression, depth: Int): Expression = βReduction(e) match {
    case x if depth == 0 => x
    case x if x == e => x
    case x => β(x, depth - 1)
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
