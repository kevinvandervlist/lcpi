package nl.soqua.lcpi.parser.repl

import nl.soqua.lcpi.ast.interpreter.ReplExpression
import org.scalatest.Matchers

trait ReplParserTester {

  protected implicit class ReplParserTester(val expr: String) extends Matchers {
    def >>(term: ReplExpression): Unit =
      ReplParser(expr).fold(ex => {
        fail(s"Parsing of REPL expression $expr failed: $ex")
      }, res => {
        res shouldBe term
      })
  }

}
