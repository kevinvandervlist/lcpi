package nl.soqua.lcpi.parser.lambda

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}
import nl.soqua.lcpi.parser.ParserError

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

trait LambdaCalcParserRules extends StdTokenParsers with PackratParsers {

  protected class LambdaLexical extends StdLexical {
    override def letter: Parser[Char] = elem("letter", c => c.isLetter && c != 'λ')
  }

  type P[+T] = PackratParser[T]

  type Tokens = LambdaLexical
  val lexical = new LambdaLexical
  lexical.delimiters ++= Seq("λ", "\\", ".", "(", ")")

  lazy val expression: P[Expression] = {
    application | lambda | variable | parentheses
  }

  lazy val lambda: P[LambdaAbstraction] = {
    ("λ" | "\\") ~> variable ~ variable.* ~ ("." ~> expression) ^^ {
      case v ~ Nil ~ e => LambdaAbstraction(v, e)
      case v ~ o ~ t => (v :: o).foldRight(t)((lit, acc) => LambdaAbstraction(lit, acc)).asInstanceOf[LambdaAbstraction]
    }
  }

  lazy val application: P[Application] = {
    expression ~ expression ^^ {
      case t ~ s => Application(t, s)
    }
  }

  lazy val variable: P[Variable] = {
    ident ^^ Variable
  }

  lazy val parentheses: P[Expression] = {
    "(" ~> expression <~ ")"
  }
}

object LambdaCalcParser extends LambdaCalcParserRules {

  def apply(source: String): Either[ParserError, Expression] = {
    parseProgram(new lexical.Scanner(source)) match {
      case NoSuccess(msg, _) => Left(ParserError(msg))
      case Success(p, _) => Right(p)
    }
  }

  lazy val parseProgram: P[Expression] = phrase(expression)
}
