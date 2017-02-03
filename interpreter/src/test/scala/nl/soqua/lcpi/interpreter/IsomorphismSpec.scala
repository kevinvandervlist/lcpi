package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.parser.lambda.LambdaCalcParser
import org.scalatest.{Assertion, Matchers, WordSpec}

import scala.language.postfixOps

class IsomorphismSpec extends WordSpec with Matchers {

  private implicit class Parse(val expr: String) extends Matchers {
    def is(o: Any): Assertion = ≅

    def ≅ : Assertion = {
      val result = for {
        p1 <- LambdaCalcParser(expr)
        p2 <- LambdaCalcParser(Transformation.asString(p1))
      } yield (p1, p2)
      result match {
        case Left(ex) => fail(s"Expression $expr failed: $ex")
        case Right((e1, e2)) => e1 shouldBe e2
      }
    }
  }

  private object isomorphic

  "parsing -> stringification -> parsing of λ-expressions" should {
    "have an isomorphic identity func" in {
      "λx.y" is isomorphic
    }
    "have an isomorphic S-combinator" in {
      "λx.λy.λz.x z (y z)" is isomorphic
    }
    "have an isomorphic Y-combinator" in {
      "λf.(λx.(f(x x)) λx.(f(x x)))" is isomorphic
    }
    "have an isomorphic truth function" in {
      "λx y.y" is isomorphic
    }
    "be isomorphic for church numeral 2" in {
      "λf.λx.f (f x)" is isomorphic
    }
    "be isomorphic with `t s` when t is a λ-abstraction" in {
      "(λx.x) y" is isomorphic
    }
  }
}
