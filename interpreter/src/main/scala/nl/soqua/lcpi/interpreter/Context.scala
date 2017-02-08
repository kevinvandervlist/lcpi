package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.{Expression, Variable}

trait Context {
  def foldLeft[T](seed: T)(op: (T, Variable, Expression) => T): T

  def foreach(fn: (Variable, Expression) => Unit): Unit

  def assign(v: Variable, e: Expression): Context

  def contains(v: Variable): Boolean
}

object Context {
  def apply(): Context = ContextImpl(Map.empty)
}

private case class ContextImpl(values: Map[Variable, Expression]) extends Context {

  override def assign(v: Variable, e: Expression): Context = copy(values + (v -> e))

  override def contains(v: Variable): Boolean = values.contains(v)

  override def foreach(fn: (Variable, Expression) => Unit): Unit = values.foreach(t => fn(t._1, t._2))

  override def foldLeft[T](seed: T)(op: (T, Variable, Expression) => T): T =
    values.foldLeft(seed)((acc, t) => op(acc, t._1, t._2))
}
