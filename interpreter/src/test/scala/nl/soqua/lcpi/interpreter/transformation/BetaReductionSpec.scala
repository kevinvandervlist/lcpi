package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class BetaReductionSpec extends WordSpec with Matchers {

  import BetaReduction.β

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "β-reduction" should {
    val a = V("a")
    val f = V("f")
    "reduce exr => value" in {
      β(A(λ(x, x), z)) shouldBe z
    }
    "reduce A(λ(x, A(f, x)), a) => A(f, a)" in {
      β(A(λ(x, A(f, x)), a)) shouldBe A(f, a)
    }
    "work with a simple function" in {
      β(A(λ(x, x), x)) shouldBe x
    }
    "work with (λx.(x x) λx.(x x)) -- should reduce to itself" in {
      val lambda = A(λ(x, A(x, x)), λ(x, A(x, x)))
      val reduced = β(lambda)
      reduced shouldBe lambda
    }
    "do multi-step β reduction" in {
      β(A(λ(x, x), A(λ(x, x), x))) shouldBe x
    }
    "beta-reduce Y-combinator properly" in {
      val Y = λ(f, λ(x, A(A(f, A(x, x)), λ(x, A(f, A(x, x))))))
      β(A(Y, z)) shouldBe A(z, A(Y, z))
    }
  }
}
