package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpec}

class InterpreterSpec extends WordSpec with Matchers {

  import Expression._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(expectedExpression: Expression)(implicit ctx: Context): Unit = {
      Interpreter(ctx, expr).fold(ex => {
        fail(s"Expression $expr failed: $ex")
      }, res => {
        withClue(
          s"""
            |expressions are not equal. l >> r:
            |got:      ${Transformation.asString(res)}
            |expected: ${Transformation.asString(expectedExpression)}
            |---
          """.stripMargin) {
          res shouldBe expectedExpression
        }
      })
    }

    def >>(expectedExpression: String)(implicit ctx: Context): Unit = Interpreter(ctx, expectedExpression)
      .fold(ex => {
        fail(s"Parsing of expected expression $expectedExpression failed: $ex")
      }, >>)
  }

  val x = V("x")
  val y = V("y")
  val z = V("z")
  val xyz = V("xyz")

  "Evaluation" should {
    implicit val ctx: Context = Context()
    "stay the same in case of an identity function" in {
      "λx.x" >> λ(x, x)
    }
    "apply a function" in {
      "(λx.x) z" >> z
    }
  }
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
      "((λa.(λb.(a (a (a b))))) (λc.(λd.(c (c d)))))" >> "(λa.(λb.(a (a (a (a (a (a (a (a b))))))))))"
    }
    "ex 10" in {
      // capture-avoiding substitution!
      "((λf.(λx.(f x))) (λy.(λx.y)))" >> "(λx.(λz.x))"
    }
  }
  "Evaluation with the usage of variables" should {
    implicit val ctx: Context = CombinatorLibrary.loadIn(Context())
    "evaluate using the stored identity function" in {
      "I z" >> z
    }
    "leverage the power of the Y-combinator" in {
      "Y z" >> A(z, A(V("I"), z))
    }
  }
}

