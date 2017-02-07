package nl.soqua.lcpi.parser.lambda

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.Matchers

trait LambdaCalcParserTester {

  protected implicit class ParserTester(val expr: String) extends Matchers {
    def >>(term: Expression): Unit =
      LambdaCalcParser(expr).fold(ex => {
        fail(s"Parsing of expression $expr failed: $ex")
      }, res => {
        res shouldBe term
      })
  }

}
