package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class StringifySpec extends WordSpec with Matchers {

  val x = V("x")
  val y = V("y")

  "Stringification of λ-expressions" should {
    "correctly stringify t s" in {
      Stringify(A(x, y)) shouldBe "(x y)"
    }
    "correctly stringify t s where t is a λx.x and s is y" in {
      Stringify(A(λ(x, x), y)) shouldBe "((λx.x) y)"
    }
  }
}
