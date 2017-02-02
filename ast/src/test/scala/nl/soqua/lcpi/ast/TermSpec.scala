package nl.soqua.lcpi.ast

import nl.soqua.lcpi.ast.lambda.{Application, LambdaAbstraction, Variable}
import org.scalatest.{Matchers, WordSpec}

class TermSpec extends WordSpec with Matchers {

  import nl.soqua.lcpi.ast.lambda.Expression._

  val x = V("x")
  val y = V("y")

  "Definition of variables" should {
    "be fine" in {
      x shouldBe a[Variable]
    }
    "be identitical" in {
      x shouldBe x
    }
    "have proper equivalence" in {
      V("x") should not be y
      y should not be x
    }
  }
  "Definition of λ" should {
    "be possible from a term and a body" in {
      λ(x, x) shouldBe a[LambdaAbstraction]
    }
  }
  "Definition of application" should {
    "be possible" in {
      val id = λ(x, x)
      A(id, y) shouldBe a[Application]
    }
  }
}
