package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

import scala.language.implicitConversions

object DeBruijnIndex {

  import AlphaReduction.α
  import Variables.free

  /**
    * Transform an expression to its De Bruijn index equivalent notation
    *
    * @param e A named lambda expression
    * @return
    */

  def index(e: Expression): Expression = {
    // First make sure that every free variable is given a unique integer value signaling its depth
    val freeMap = free(α(e)).foldLeft(Map.empty[Variable, Int])({
      case (m, v) if m.contains(v) => m
      case (m, v) => m + (v -> (m.size + 1))
    })
    index(e, freeMap.size + 1, freeMap)
  }

  def reification(e: Expression): Expression = ???

  private val empty = Variable("")

  private def index(e: Expression, level: Int, vars: Map[Variable, Int]): Expression = e match {
    case v: Variable => vars.get(v).map(x => level - x).map(x => Variable(x.toString)).getOrElse(empty)
    case Application(s, t) => Application(index(s, level, vars), index(t, level, vars))
    case LambdaAbstraction(x, t) => LambdaAbstraction(empty, index(t, level + 1, vars + (x -> level)))
  }
}
