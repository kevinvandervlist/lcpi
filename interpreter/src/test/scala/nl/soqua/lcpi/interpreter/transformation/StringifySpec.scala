package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class StringifySpec extends WordSpec with Matchers {

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Stringification of λ-expressions" should {
    "correctly stringify t s" in {
      Stringify(A(x, y)) shouldBe "x y"
    }
    "correctly stringify t s where t is a λx.x and s is y" in {
      Stringify(A(λ(x, x), y)) shouldBe "(λx.x) y"
    }
    "left-recursive with application" in {
      Stringify(A(A(x, y), z)) shouldBe "x y z"
    }
    "S-expression" in {
      Stringify(λ(x, λ(y, λ(z, A(A(x, z), A(y, z)))))) shouldBe "λx.λy.λz.x z (y z)"
    }
  }
}
