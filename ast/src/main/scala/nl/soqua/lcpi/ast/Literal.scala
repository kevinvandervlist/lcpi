package nl.soqua.lcpi.ast

import scala.util.parsing.input.Positional


object Literal {
  def apply(name: String): Literal = StringLiteral(name)
}

sealed trait Literal extends Positional

private case class StringLiteral(name: String) extends Literal