package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

import scala.util.Try

object DeBruijn {

  import AlphaReduction.α
  import Names.unused
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

  def reify(e: Expression): Expression = reify(e, 0, Map())._1

  private def asInt(v: Variable): Int = Try {
    v.symbol.toInt
  }.getOrElse(Integer.MAX_VALUE)

  private def reify(e: Expression, level: Int, vars: Map[Int, Variable]): (Expression, Map[Int, Variable]) = e match {
    case v: Variable if vars.contains(level - asInt(v)) => (vars(level - asInt(v)), vars)
    case _: Variable => (unused(vars.values.toList), vars)
    case Application(s, t) =>
      val (s1, vars1) = reify(s, level, vars)
      val (s2, vars2) = reify(t, level, vars1)
      (Application(s1, s2), vars2)
    case LambdaAbstraction(_, t) =>
      val v = unused(vars.values.toList)
      val (t1, vars1) = reify(t, level + 1, vars + (level -> v))
      (LambdaAbstraction(v, t1), vars1)
  }

  private val empty = Variable("")

  private def index(e: Expression, level: Int, vars: Map[Variable, Int]): Expression = e match {
    case v: Variable => vars.get(v)
      .map(x => level - x)
      .map(x => Variable(x.toString))
      // It should always get a value, not finding one should be _impossible_, so crash if we don't have one
      .get
    case Application(s, t) => Application(index(s, level, vars), index(t, level, vars))
    case LambdaAbstraction(x, t) => LambdaAbstraction(empty, index(t, level + 1, vars + (x -> level)))
  }
}
