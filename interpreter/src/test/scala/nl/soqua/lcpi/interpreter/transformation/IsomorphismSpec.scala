package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.A
import nl.soqua.lcpi.interpreter.transformation.Stringify._
import nl.soqua.lcpi.parser.lambda.LambdaCalcParser
import org.scalatest.{Assertion, Matchers, WordSpec}

import scala.language.postfixOps

class IsomorphismSpec extends WordSpec with Matchers {

  import DeBruijnIndex._

  private implicit class Parse(val expr: String) extends Matchers {
    def is(o: Any): Assertion = o match {
      case `plain` => plainIsomorphism
      case `deBruijn` => plainIsomorphism
    }

    def plainIsomorphism: Assertion = {
      val result = for {
        p1 <- LambdaCalcParser(expr)
        p2 <- LambdaCalcParser(p1)
      } yield (p1, p2)
      result match {
        case Left(ex) => fail(s"Expression $expr failed: $ex")
        case Right((e1, e2)) => e1 shouldBe e2
      }
    }

    def deBruijnIsomorphism: Assertion = {
      val result = for {
        p1 <- LambdaCalcParser(expr)
        p2 <- LambdaCalcParser(reify(index(p1)))
      } yield (p1, p2)
      result match {
        case Left(ex) => fail(s"Expression $expr failed: $ex")
        case Right((e1, e2)) => e1 shouldBe e2
      }
    }
  }

  sealed trait isomorphic

  private object plain extends isomorphic

  private object deBruijn extends isomorphic

  "isomorphism of parsing -> stringification -> parsing of λ-expressions" should {
    "hold for identity func" in {
      "λx.x" is plain
    }
    "hold for S-combinator" in {
      "λx.λy.λz.x z (y z)" is plain
    }
    "hold for Y-combinator" in {
      "λf.(λx.(f(x x)) λx.(f(x x)))" is plain
    }
    "hold for truth function" in {
      "λx y.y" is plain
    }
    "hold for church numeral 2" in {
      "λf.λx.f (f x)" is plain
    }
    "hold for `t s` when t is a λ-abstraction" in {
      "(λx.x) y" is plain
    }
  }
  "isomorphism of parsing -> De Bruijn Index -> reification -> stringification -> parsing of λ-expressions" should {
    "hold for the identity func" in {
      "λa.a" is deBruijn
    }
    "hold for an expression with a free variable" in {
      "λa.b" is deBruijn
    }
    "hold for a more complex function" in {
      "λa.(λb.(b λc.c) λd.(a d))" is deBruijn
    }
  }
}
