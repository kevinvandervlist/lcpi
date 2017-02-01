package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.{Application, Expression, LambdaAbstraction, Variable}

object Transformation {

  import Names._

  /**
    * Alpha reduction. Rewrite expressions so that every variable is unique.
    *
    * @param e The expression to rewrite
    * @return A rewritten expression.
    */
  def α(e: Expression): Expression = α(e, List.empty)

  private def α(e: Expression, encountered: List[Variable]): Expression = e match {
    case v: Variable => v
    case Application(t, s) => Application(α(t, encountered), α(s, encountered))
    case LambdaAbstraction(x, a) if encountered contains x =>
      val replacement = unused(x, encountered)
      α(LambdaAbstraction(replacement, sub(a, x, replacement)), encountered)
    case LambdaAbstraction(x, a) => LambdaAbstraction(x, α(a, x :: encountered))
  }

  /**
    * Extract all variables from a lambda expression
    *
    * @param e The expression
    * @return A list of variables
    */
  def vars(e: Expression): List[Variable] = e match {
    case v: Variable => List(v)
    case Application(t, s) => vars(t) union vars(s)
    case LambdaAbstraction(x, a) => x :: vars(a)
  }

  /**
    * Find a list of free variables in a given expression
    *
    * @param e The expression to find free variables in
    * @return A list of variables that are free.
    */
  def free(e: Expression): List[Variable] = e match {
    case v: Variable => List(v)
    case Application(t, s) => free(t) union free(s)
    case LambdaAbstraction(x, a) => free(a) filterNot (v => v == x)
  }

  /**
    * alias for `sub`
    */
  def substitute(e: Expression, v: Variable, s: Expression): Expression = sub(e, v, s)

  /**
    * Given expression 'e', substitute a free variable 'v' with the new expression 's'
    *
    * @param e The input expression
    * @param v A variable to substitute
    * @param s A substitute that will replace v
    */
  def sub(e: Expression, v: Variable, s: Expression): Expression = e match {
    case vp: Variable if vp == v => s
    case vp: Variable => vp
    case Application(l, r) => Application(sub(l, v, s), sub(r, v, s))
    case lam@LambdaAbstraction(x, _) if x == v => lam
    case LambdaAbstraction(x, le) => LambdaAbstraction(x, sub(le, v, s))
  }

}
