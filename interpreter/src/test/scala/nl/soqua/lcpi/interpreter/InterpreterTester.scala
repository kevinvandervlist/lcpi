package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.interpreter.transformation.{DeBruijnIndex, Stringify}
import org.scalatest.Matchers

trait InterpreterTester {

  /**
    * This class allows a convenient notation for test declarations
    * @param expr The left hand side expression to parse. Yields the 'actual' result of the assertion.
    */
  protected implicit class InterpreterParserAndTester(val expr: String) extends Matchers {
    /**
      * Assert equality given an expression expr that will be compared to the 'actual' result
      * (e.g. the left hand side). Equality is determined by writing the expressions in De Bruijn Index form
      * for α-equivalence
      * @param expectedExpression
      * @param ctx
      */
    def >>(expectedExpression: Expression)(implicit ctx: Context): Unit = {
      Interpreter(ctx, expr).fold(ex => {
        fail(s"Expression $expr failed: $ex")
      }, actualExpression => {
        withClue(
          s"""
             |expressions are not equal. l >> r:
             |got:      ${Stringify(actualExpression)}
             |expected: ${Stringify(expectedExpression)}
             |---
          """.stripMargin) {
          DeBruijnIndex.index(actualExpression) shouldBe DeBruijnIndex.index(expectedExpression)
        }
      })
    }

    /**
      * Assert equality given an expression str that will be parsed and compared to the 'actual' result
      * (e.g. the left hand side). Equality is determined by writing the expressions in De Bruijn Index form
      * for α-equivalence
      * @param expectedExpression
      * @param ctx
      */
    def >>(expectedExpression: String)(implicit ctx: Context): Unit = Interpreter(ctx, expectedExpression)
      .fold(ex => {
        fail(s"Parsing of expected expression $expectedExpression failed: $ex")
      }, >>)
  }

}
