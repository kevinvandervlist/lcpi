package nl.soqua.lcpi.parser.lambda

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}
import nl.soqua.lcpi.parser.ParserError

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

trait LambdaCalcParserRules extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]

  private val gap: Regex = "\\s+".r

  lazy val parseVariable: P[Variable] = "[a-zA-Z0-9]+".r ^^ (str => Variable(str))

  lazy val expression: P[Expression] = {
    parseApplication | parenthesizedExpression | parseLambda | parseVariable
  }

  lazy val parenthesizedExpression: P[Expression] = "(" ~> expression <~ ")"

  lazy val parseApplication: P[Expression] = {
    val spaceSeparated = (expression <~ gap) ~ expression ^^ { case t ~ s => Application(t, s) }
    val disambiguationForcedByQuotes = expression ~ ("(" ~> expression <~ ")") ^^ { case t ~ s => Application(t, s) }
    spaceSeparated | disambiguationForcedByQuotes
  }

  lazy val parseLambda: P[Expression] = {
    ("λ" | "\\") ~> ((parseVariable ~ (gap ~> parseVariable).*) <~ ".") ~ expression ^^ {
      case v ~ Nil ~ t => LambdaAbstraction(v, t)
      case v ~ o ~ t => (v :: o).foldRight(t)((lit, acc) => LambdaAbstraction(lit, acc))
    }
  }
}

object LambdaCalcParser extends RegexParsers with LambdaCalcParserRules {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[\t\r\f\n]+".r

  def apply(source: String): Either[ParserError, Expression] = {
    parse(parseProgram, new PackratReader(new CharSequenceReader(source))) match {
      case NoSuccess(msg, next) => Left(ParserError(msg, next.pos))
      case Success(p, _) => Right(p)
    }
  }

  lazy val parseProgram: P[Expression] = phrase(expression)
}
