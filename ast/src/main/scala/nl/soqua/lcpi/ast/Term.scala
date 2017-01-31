package nl.soqua.lcpi.ast

import scala.util.parsing.input.Positional

object Term {
  def V(symbol: Literal): Variable = Variable(symbol)

  def λ(symbol: Literal, body: Term): Lambda = Lambda(symbol, body)

  def A(t: Term, s: Term): Term = Application(t, s)
}

sealed trait Term extends Positional

case class Variable(symbol: Literal) extends Term

case class Lambda(variable: Literal, body: Term) extends Term

case class Application(t: Term, s: Term) extends Term

