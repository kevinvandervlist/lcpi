package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

object Transformation {

  import Names._

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
      α(LambdaAbstraction(replacement, sub(a, x, replacement)), encountered)
    case LambdaAbstraction(x, a) => LambdaAbstraction(x, α(a, x :: encountered))
  }

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
      βReduction(x)
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
    case Application(LambdaAbstraction(name, body), t) => sub(body, name, t)
    case Application(s, t) => Application(βReduction(s), βReduction(t))
  }

  def η(e: Expression): Expression = e match {
    case v: Variable => v
    case LambdaAbstraction(x, t) => LambdaAbstraction(x, η(t))
    case Application(LambdaAbstraction(n1, Application(expr, n2)), arg) if n1 == n2 => Application(η(expr), η(arg))
    case Application(s, t) => Application(η(s), η(t))
  }

  def eta(e: Expression): Expression = η(e)

  def asString(e: Expression): String = e match {
    case v: Variable => v.symbol
    case Application(t, s) => s"(${asString(t)} ${asString(s)})"
    case LambdaAbstraction(x, a) => s"λ${asString(x)}.${asString(a)}"
  }

  /**
    * Extract all variables from a lambda expression
    *
    * @param e The expression
    * @return A Set of variables
    */
  def vars(e: Expression): Set[Variable] = e match {
    case v: Variable => Set(v)
    case Application(t, s) => vars(t) union vars(s)
    case LambdaAbstraction(x, a) => vars(a) + x
  }

  /**
    * Find a Set of free variables in a given expression
    *
    * @param e The expression to find free variables in
    * @return A Set of variables that are free.
    */
  def free(e: Expression): Set[Variable] = e match {
    case v: Variable => Set(v)
    case Application(t, s) => free(t) union free(s)
    case LambdaAbstraction(x, a) => free(a) filterNot (v => v == x)
  }

  /**
    * Return a Set of all bound variables of an expression
    *
    * @param e The expression to analyze
    * @return A Set of bound variables
    */
  def bound(e: Expression): Set[Variable] = vars(e) diff free(e)

  /**
    * alias for `sub`
    */
  private[interpreter] def substitute(e: Expression, v: Variable, s: Expression): Expression = sub(e, v, s)

  /**
    * Given expression 'e', substitute a free variable 'v' with the new expression 's'
    *
    * @param e The input expression
    * @param v A variable to substitute
    * @param s A substitute that will replace v
    */
  private[interpreter] def sub(e: Expression, v: Variable, s: Expression): Expression = e match {
    case vp: Variable if vp == v => s
    case vp: Variable => vp
    case Application(l, r) => Application(sub(l, v, s), sub(r, v, s))
    case lam@LambdaAbstraction(x, _) if x == v => lam
    case LambdaAbstraction(x, le) => LambdaAbstraction(x, sub(le, v, s))
  }

}
