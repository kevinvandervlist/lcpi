package nl.soqua.lcpi.parser.repl

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpecLike}

class ReplParserSpec extends ReplParserTester with WordSpecLike with Matchers {

  import Expression._

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
