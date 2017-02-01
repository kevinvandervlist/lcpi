package nl.soqua.lcpi.parser

import nl.soqua.lcpi.ast._

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]

  override def skipWhitespace = true

  override val whiteSpace: Regex = "[\t\r\f\n]+".r

  private val gap = "\\s+".r

  def apply(source: String): Either[ParserError, Expression] = {
    parse(parseProgram, new PackratReader(new CharSequenceReader(source))) match {
      case NoSuccess(msg, next) => Left(ParserError(msg, next.pos))
      case Success(p, _) => Right(p)
    }
  }

  lazy val parseProgram: P[Expression] = positioned {
    phrase(expression)
  }

  lazy val expression: P[Expression] = positioned {
    parseApplication | parenthesizedExpression | parseLambda | parseVariable
  }

  lazy val parenthesizedExpression: P[Expression] = positioned {
    "(" ~> expression <~ ")"
  }

  lazy val parseVariable: P[Variable] = positioned {
    "[a-z][a-zA-Z0-9]*".r ^^ (str => Variable(str))
  }

  lazy val parseApplication: P[Expression] = positioned {
    val spaceSeparated = (expression <~ gap) ~ expression ^^ { case t ~ s => Application(t, s) }
    val disambiguationForcedByQuotes = expression ~ ("(" ~> expression <~ ")") ^^ { case t ~ s => Application(t, s) }
    spaceSeparated | disambiguationForcedByQuotes
  }

  lazy val parseLambda: P[Expression] = positioned {
    ("λ" | "\\") ~> ((parseVariable ~ (gap ~> parseVariable).*) <~ ".") ~ expression ^^ {
      case v ~ Nil ~ t => LambdaAbstraction(v, t)
      case v ~ o ~ t => (v :: o).foldRight(t)((lit, acc) => LambdaAbstraction(lit, acc))
    }
  }
}
