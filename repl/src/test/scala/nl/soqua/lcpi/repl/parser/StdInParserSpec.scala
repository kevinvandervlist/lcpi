package nl.soqua.lcpi.repl.parser

import nl.soqua.lcpi.ast.lambda.{LambdaAbstraction, Variable}
import nl.soqua.lcpi.repl.monad.ReplMonad
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, WordSpecLike}

class StdInParserSpec extends StdInParserTester with WordSpecLike with Matchers {
  "A std in parser" should {
    "parse the standard commands" in {
      List(
        "help" -> ReplMonad.help(),
        "quit" -> ReplMonad.quit(),
        "show" -> ReplMonad.show(),
        "reset" -> ReplMonad.reset(),
        "trace" -> ReplMonad.trace()
      ) foreach {
        case (input, output) => input >> output
      }
    }
    "parse an expression" in {
      "λx.x" >> ReplMonad.expression(LambdaAbstraction(Variable("x"), Variable("x")))
    }
    "don't parse anything that is not a valid λ expression" in {
      assertThrows[TestFailedException] {
        "# foo" >> ReplMonad.help()
      }
    }
  }
}
