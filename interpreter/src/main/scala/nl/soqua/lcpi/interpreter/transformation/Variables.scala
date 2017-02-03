package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

object Variables {
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
    * Is the provided variable bound in the given expression?
    *
    * @param v The variable to check
    * @param e The λ-expression to check
    * @return A boolean indicating whether the variable is bound or not.
    */
  def isBound(v: Variable, e: Expression): Boolean = bound(e) contains v
}
