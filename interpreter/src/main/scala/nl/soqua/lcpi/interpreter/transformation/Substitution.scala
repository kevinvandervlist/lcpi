package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

object Substitution {
  /**
    * alias for `sub`
    */
  private[transformation] def substitute(e: Expression, v: Variable, s: Expression): Expression = sub(e, v, s)

  /**
    * Given expression 'e', substitute a free variable 'v' with the new expression 's'
    *
    * @param e The input expression
    * @param v A variable to substitute
    * @param s A substitute that will replace v
    */
  private[transformation] def sub(e: Expression, v: Variable, s: Expression): Expression = e match {
    case vp: Variable if vp == v => s
    case vp: Variable => vp
    case Application(l, r) => Application(sub(l, v, s), sub(r, v, s))
    case lam@LambdaAbstraction(x, _) if x == v => lam
    case LambdaAbstraction(x, le) => LambdaAbstraction(x, sub(le, v, s))
  }

}
