package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpecLike}

class CodeGolfSpec extends InterpreterTester with WordSpecLike with Matchers {

  import Expression._

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Codegolf testsuite" should {
    // http://codegolf.stackexchange.com/questions/284/write-an-interpreter-for-the-untyped-lambda-calculus
    implicit val ctx: Context = Context()
    "ex 1" in {
      "((λx.x) (λy.(λz.z)))" >> "(λy.(λz.z))"
    }
    "ex 2" in {
      "(λx.((λy.y) x))" >> "λx.x"
    }
    "ex 3" in {
      "((λx.(λy.x)) (λa.a))" >> "(λy.(λa.a))"
    }
    "ex 4" in {
      "(((λx.(λy.x)) (λa.a)) (λb.b))" >> "λa.a"
    }
    "ex 5" in {
      "((λx.(λy.y)) (λa.a))" >> "λy.y"
    }
    "ex 6" in {
      "(((λx.(λy.y)) (λa.a)) (λb.b))" >> "λb.b"
    }
    "ex 7" in {
      "((λx.(x x)) (λx.(x x)))" >> "(x (λx.(x x)))"
      // anything goes, no normal form
    }
    "ex 8" in {
      "(((λx.(λy.x)) (λa.a)) ((λx.(x x)) (λx.(x x))))" >> "λa.a"
      // (This is an example of an expression which does not normalize if you evaluate the arguments before the
      // function call, and sadly an example for which my attempted solution fails)
    }
    "ex 9" in {
      "((λa.(λb.(a (a (a b))))) (λc.(λd.(c (c d)))))" >> "(λb.(λd.(b (b (b (b (b (b (b (b d))))))))))"
    }
    "ex 10" in {
      // capture-avoiding substitution!
      "((λf.(λx.(f x))) (λy.(λx.y)))" >> "(λx.(λz.x))"
    }
  }
}

