package nl.soqua.lcpi.parser.repl

import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.Variable
import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.lambda.LambdaCalcParserRules

trait ReplParserRules extends LambdaCalcParserRules {
  lexical.delimiters ++= Seq(":=")

  lazy val line: P[ReplExpression] = {
    (variable <~ ":=").? ~ expression ^^ {
      case None ~ expr => ReplExpression.e2re(expr)
      case Some(v) ~ expr => Assignment(v, expr)
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
