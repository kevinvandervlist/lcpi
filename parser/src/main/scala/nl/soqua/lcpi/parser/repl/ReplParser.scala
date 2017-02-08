package nl.soqua.lcpi.parser.repl

import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.Variable
import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.lambda.LambdaCalcParserRules

trait ReplParserRules extends LambdaCalcParserRules {
  lexical.delimiters ++= Seq(":=")

  private def capitalizationRequired(v: Variable) = s"The variable '${v.symbol}' should be fully capitalized"

  lazy val line: P[ReplExpression] = {
    (variable <~ ":=").? ~ expression >> {
      case None ~ expr => success(ReplExpression.e2re(expr))
      case Some(v) ~ _ if v.symbol.toUpperCase() != v.symbol => failure(capitalizationRequired(v))
      case Some(v) ~ expr => success(Assignment(v, expr))
    }
  }
}

object ReplParser extends ReplParserRules {

  def apply(source: String): Either[ParserError, ReplExpression] = {
    parseLine(new lexical.Scanner(source)) match {
      case NoSuccess(msg, _) => Left(ParserError(msg))
      case Success(p, _) => Right(p)
    }
  }

  lazy val parseLine: P[ReplExpression] = phrase(line)
}
