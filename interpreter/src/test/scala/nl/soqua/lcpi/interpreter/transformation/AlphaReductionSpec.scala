package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class AlphaReductionSpec extends WordSpec with Matchers {

  import AlphaReduction.α

  val f = V("f")
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
    "capture-avoiding substitution" in {
      val e = A(λ(f, λ(x, A(f, x))), λ(y, λ(x, y)))
      val alpha = AlphaReduction.α(e)
      val expected = A(λ(f, λ(x, A(f, x))), λ(y, λ(z, y)))
      withClue(
        s"""
           |expressions are not equal. l >> r:
           |got:      ${Stringify(alpha)}
           |expected: ${Stringify(expected)}
           |---
          """.stripMargin) {
        DeBruijnIndex.index(alpha) shouldBe DeBruijnIndex.index(expected)
      }
    }
  }
}
