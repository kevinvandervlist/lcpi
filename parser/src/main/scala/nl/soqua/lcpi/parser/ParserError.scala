package nl.soqua.lcpi.parser

import scala.util.parsing.input.{NoPosition, Position}

sealed trait ParserError {
  val message: String
  val position: Position
}

object ParserError {
  def apply(msg: String): ParserError = ParserErrorImpl(msg, NoPosition)

  def apply(msg: String, pos: Position) = ParserErrorImpl(msg, pos)

  def unapply(arg: ParserError): Option[(String, Position)] = Some((arg.message, arg.position))
}

case class ParserErrorImpl(override val message: String, override val position: Position) extends ParserError
