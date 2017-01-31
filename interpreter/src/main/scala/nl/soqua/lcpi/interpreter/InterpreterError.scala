package nl.soqua.lcpi.interpreter

import scala.util.parsing.input.{NoPosition, Position}

sealed trait InterpreterError {
  val message: String
  val position: Position
}

object InterpreterError {
  def apply(msg: String): InterpreterError = InterpreterErrorImpl(msg, NoPosition)

  def apply(msg: String, pos: Position) = InterpreterErrorImpl(msg, pos)
}

case class InterpreterErrorImpl(override val message: String, override val position: Position) extends InterpreterError
