package nl.soqua.lcpi.parser.repl

import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.Variable
import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.lambda.LambdaCalcParserRules

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

trait ReplParserRules extends RegexParsers with PackratParsers with LambdaCalcParserRules {
  private val assignmentToken: Regex = "\\s*:=\\s".r

  lazy val line: P[ReplExpression] = {
    ("[A-Z]+".r <~ assignmentToken).? ~ expression ^^ {
      case None ~ expr => ReplExpression.e2re(expr)
      case Some(name: String) ~ expr => Assignment(Variable(name), expr)
    }
  }
}

object ReplParser extends ReplParserRules {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[\t\r\f\n]+".r

  def apply(source: String): Either[ParserError, ReplExpression] = {
    parse(parseLine, new PackratReader(new CharSequenceReader(source))) match {
      case NoSuccess(msg, _) => Left(ParserError(msg))
      case Success(p, _) => Right(p)
    }
  }

  lazy val parseLine: P[ReplExpression] = phrase(line)
}
