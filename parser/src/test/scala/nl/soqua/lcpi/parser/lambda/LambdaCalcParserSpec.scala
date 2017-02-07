package nl.soqua.lcpi.parser.lambda

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, WordSpecLike}

class LambdaCalcParserSpec extends LambdaCalcParserTester with WordSpecLike with Matchers {

  import Expression._

  val arg = V("a")
  val f = V("f")
  val x = V("x")
  val y = V("y")
  val z = V("z")
  val xyz = V("xyz")

  "A parser" should {
    "parse the identity func with `\\` notation" in {
      "\\x.x" >> λ(x, x)
    }
    "fail to parse an invalid definition" in {
      assertThrows[TestFailedException] {
        "λ(y,y)" >> x
      }
    }
    "parse a simple variable" in {
      "x" >> x
    }
    "parse a longer variable" in {
      "xyz" >> xyz
    }
    "parse an identity function" in {
      "\\xyz.xyz" >> λ(xyz, xyz)
    }
    "parse a nested function" in {
      "λx.λx.x" >> λ(x, λ(x, x))
    }
    "parse functions with shorthand of multiple parameters" in {
      "λx y.x" >> λ(x, λ(y, x))
    }
    "parse nested functions with shorthands" in {
      "λx y z.x" >> λ(x, λ(y, λ(z, x)))
    }
    "parse a constant function" in {
      "λx.y" >> λ(x, y)
    }
    "parse a parenthesized function" in {
      "(λx.((λy.z)))" >> λ(x, λ(y, z))
    }
    "parse a simple application" in {
      "(x y)" >> A(x, y)
    }
    "parse a more complex example of application" in {
      "((λx.x) (λy.y))" >> A(λ(x, x), λ(y, y))
    }
    "parse a more complex example of application without parens" in {
      "(λx.x) (λy.y)" >> A(λ(x, x), λ(y, y))
    }
    "parse application of a function and a variable" in {
      "λx.(x y)" >> λ(x, A(x, y))
    }
    "parse applications left-associative" in {
      "λf.x y z" >> λ(f, A(A(x, y), z))
    }
    "parse applications left-associative, even when given more complex λ-terms" in {
      "λx.x λy.y λz.z f" >> λ(x, A(x, λ(y, A(y, λ(z, A(z, f))))))
    }
  }
  "Church numerals" should {
    "parse 0" in {
      "λf.λx.x" >> λ(f, λ(x, x))
    }
    "parse 1" in {
      "λf.λx.f x" >> λ(f, λ(x, A(f, x)))
    }
    "parse 2" in {
      "λf.λx.f (f x)" >> λ(f, λ(x, A(f, A(f, x))))
    }
    "parse 2 (condensed notation)" in {
      "λf x.f(f x)" >> λ(f, λ(x, A(f, A(f, x))))
    }
  }
  "Well known functions" should {
    "[I] parse an identity function" in {
      "λx.x" >> λ(x, x)
    }
    "self-apply" in {
      "λx.(x x)" >> λ(x, A(x, x))
    }
    "apply" in {
      "λf.λa.(f a)" >> λ(f, λ(arg, A(f, arg)))
    }
    "[T] truth combinator" in {
      "λx y.x" >> λ(x, λ(y, x))
    }
    "[F] truth combinator" in {
      "λx y.y" >> λ(x, λ(y, y))
    }
    "[K] parse a K-function" in {
      "λx.λy.x" >> λ(x, λ(y, x))
    }
    "[S] parse an S-function" in {
      "λx.λy.λz.x z (y z)" >> λ(x, λ(y, λ(z, A(A(x, z), A(y, z)))))
    }
    "[Y] parse the y-combinator function" in {
      val g = V("g")
      //"λf.(λx.(f(x x)) λx.(f(x x)))"
      // TODO: Validate the y-combinator, especially the AST
      "λg.(λx.g (x x)) (λx.g (x x))" >> λ(g, A(λ(x, A(g, A(x, x))), λ(x, A(g, A(x, x)))))
    }
  }
}
