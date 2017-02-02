package nl.soqua.lcpi.parser.repl

import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpec}

class ReplParserSpec extends WordSpec with Matchers {

  import Expression._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(term: ReplExpression): Unit =
      ReplParser(expr).fold(ex => {
        fail(s"Parsing of REPL expression $expr failed: $ex")
      }, res => {
        res shouldBe term
      })
  }

  val I = V("I")
  val x = V("x")

  "A REPL parser" should {
    "parse an identity function" in {
      "λx.x" >> λ(x, x)
    }
    "parse an identity function assignment" in {
      "I := λx.x" >> Assignment(I, λ(x, x))
    }
  }
}
