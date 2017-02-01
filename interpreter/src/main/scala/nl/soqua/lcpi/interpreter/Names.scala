package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.Variable

object Names {
  private val preferedAlphabet: List[Variable] = ('a' to 'z').map(c => Variable(c.toString)).toList

  /**
    * Come up with an unnamed variable
    *
    * @param v     The variable that needs to be replaced
    * @param known All currently known variables
    * @return A new variable name.
    */
  def unused(v: Variable, known: List[Variable]): Variable =
    preferedAlphabet
      .find(c => !known.contains(c)) match {
      case Some(unused) => unused
      case None => uniqueSuffix(v, known)
    }

  /**
    * Generate a new number based on a unique suffix (e.g. x0, x1, ...)
    *
    * @param v     The variable to rewrite
    * @param known All currently known values
    * @return A new variable name
    */
  def uniqueSuffix(v: Variable, known: List[Variable]): Variable = Stream
      .from(0)
      .find(p => !known.contains(newVar(v, p)))
      .map(p => newVar(v, p))
      .get

  private def newVar(v: Variable, n: Int): Variable = Variable(s"${v.symbol}$n")
}
