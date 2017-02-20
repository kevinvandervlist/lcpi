package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.{Expression, Variable}

trait Context {
  protected def expressions: List[(Variable, Expression)]

  def assign(v: Variable, e: Expression): Context

  def contains(v: Variable): Boolean

  def foldLeft[T](seed: T)(op: (T, Variable, Expression) => T): T =
    expressions.foldLeft(seed)((acc, t) => op(acc, t._1, t._2))

  def foreach(fn: (Variable, Expression) => Unit): Unit =
    expressions.foreach(t => fn(t._1, t._2))

  def map[B](fn: (Variable, Expression) => B): List[B] =
    expressions.map(t => fn(t._1, t._2))
}

object Context {
  def apply(): Context = ContextImpl(Map.empty)
}

private case class ContextImpl(values: Map[Variable, Expression]) extends Context {

  protected def expressions: List[(Variable, Expression)] = values.toList
    .sortWith((t1, t2) => {
      t1._1.symbol < t2._1.symbol
    })

  override def assign(v: Variable, e: Expression): Context = copy(values + (v -> e))

  override def contains(v: Variable): Boolean = values.contains(v)
}
