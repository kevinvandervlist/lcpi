package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{V, λ}
import org.scalatest.{Matchers, WordSpec}

class AlphaReductionSpec extends WordSpec with Matchers {

  import AlphaReduction.α

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "α-reduction" should {
    val a = V("a")
    val b = V("b")

    "work" in {
      α(λ(x, λ(x, x))) shouldBe λ(x, λ(a, a))
    }
    "work too" in {
      α(λ(x, λ(x, λ(x, x)))) shouldBe λ(x, λ(a, λ(b, b)))
    }
  }
}
