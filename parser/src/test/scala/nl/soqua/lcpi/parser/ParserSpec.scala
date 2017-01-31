package nl.soqua.lcpi.parser

import nl.soqua.lcpi.ast.{Literal, Term}
import org.scalatest.{Matchers, WordSpec}

class ParserSpec extends WordSpec with Matchers {

  import nl.soqua.lcpi.ast.Term._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(term: Term): Unit =
      Parser(expr).fold(ex => {
        fail(s"Parsing of expression $expr failed: $ex")
      }, res => {
        res shouldBe term
      })
  }

  val x = Literal("x")
  val y = Literal("y")
  val z = Literal("z")
  val xyz = Literal("xyz")

  "A parser" should {
    "abc" in {
      //      "(λx.λy.x)(λy.y)(λx.x)" >> V(x)
      "\\x.x" >> λ(x, V(x))
    }
    "parse a simple variable" in {
      "x" >> V(x)
    }
    "parse a longer variable" in {
      "xyz" >> V(xyz)
    }
    "parse an identity function" in {
      "\\xyz.xyz" >> λ(xyz, V(xyz))
    }
    "parse a nested function" in {
      "λx.λx.x" >> λ(x, λ(x, V(x)))
    }
    "parse a constant function" in {
      "λx.y" >> λ(x, V(y))
    }
    "parse a parenthesized function" in {
      "(λx.((λy.z)))" >> λ(x, λ(y, V(z)))
    }
    "parse a simple application" in {
      "(x y)" >> A(V(x), V(y))
    }
    "parse a more complex example of application" in {
      "((λx.x) (λy.y))" >> A(λ(x, V(x)), λ(y, V(y)))
    }
    "parse a more complex example of application without parens" in {
      "(λx.x) (λy.y)" >> A(λ(x, V(x)), λ(y, V(y)))
    }
    "parse application of a function and a variable" in {
      "λx.(x y)" >> λ(x, A(V(x), V(y)))
    }
  }
  "Church numerals" should {
    val f = Literal("f")
    "parse 0" in {
      "λf.λx.x" >> λ(f, λ(x, V(x)))
    }
    "parse 1" in {
      "λf.λx.f x" >> λ(f, λ(x, A(V(f), V(x))))
    }
    "parse 2" in {
      "λf.λx.f (f x)" >> λ(f, λ(x, A(V(f), A(V(f), V(x)))))
    }
    "parse 2 (condensed notation)" in {
      "λf.λx.f(f x)" >> λ(f, λ(x, A(V(f), A(V(f), V(x)))))
    }
  }
  "Well known functions" should {
    "[I] parse an identity function" in {
      "λx.x" >> λ(x, V(x))
    }
    "[Y] parse the y-combinator function" in {
      val g = Literal("g")
      "λg.(λx.g (x x)) (λx.g (x x))" >> λ(g, A(λ(x, A(V(g), A(V(x), V(x)))), λ(x, A(V(g), A(V(x), V(x))))))
    }
  }
}

//I := λx.x
//K := λx.λy.x
//S := λx.λy.λz.x z (y z)
//B := λx.λy.λz.x (y z)
//C := λx.λy.λz.x z y
//W := λx.λy.x y y
//U := λx.λy.y (x x y)
//ω := λx.x x
//Ω := ω ω
//Y := λg.(λx.g (x x)) (λx.g (x x))