package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.{Expression, Variable}

import scala.collection.mutable

trait Context {
  def foreach(fn: (Variable, Expression) => Unit): Unit

  def assign(v: Variable, e: Expression): Option[Expression]

  def contains(v: Variable): Boolean
}

object Context {
  def apply(): Context = new ContextImpl()
}

private class ContextImpl() extends Context {
  private val values: mutable.Map[Variable, Expression] = mutable.Map()

  override def assign(v: Variable, e: Expression): Option[Expression] = values.put(v, e)

  override def contains(v: Variable): Boolean = values.contains(v)

  override def foreach(fn: (Variable, Expression) => Unit): Unit = values.foreach(t => fn(t._1, t._2))
}
