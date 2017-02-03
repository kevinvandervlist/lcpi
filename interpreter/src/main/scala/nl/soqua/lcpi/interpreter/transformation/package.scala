package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.{Expression, Variable}

package object transformation {
  def α(e: Expression): Expression = AlphaReduction.α(e)

  def β(e: Expression): Expression = BetaReduction.β(e)

  def η(e: Expression): Expression = EtaReduction.η(e)

  def substitute(e: Expression, v: Variable, s: Expression): Expression = Substitution.substitute(e, v, s)
}
