package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

import scala.language.implicitConversions

object DeBruijnIndex {

  import AlphaReduction.α
  import Variables.free

  def deBruijn(e: Expression): Expression = {
    // First make sure that every free variable is given a unique integer value signaling its depth
    val freeMap = free(α(e)).foldLeft(Map.empty[Variable, Int])({
      case (m, v) if m.contains(v) => m
      case (m, v) => m + (v -> (m.size + 1))
    })
    deBruijn(e, freeMap.size + 1, freeMap)
  }

  private val empty = Variable("")

  private def deBruijn(e: Expression, level: Int, index: Map[Variable, Int]): Expression = e match {
    case v: Variable => index.get(v).map(x => level - x).map(x => Variable(x.toString)).getOrElse(empty)
    case Application(s, t) => Application(deBruijn(s, level, index), deBruijn(t, level, index))
    case LambdaAbstraction(x, t) => LambdaAbstraction(empty, deBruijn(t, level + 1, index + (x -> level)))
  }

}
