package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class EtaReductionSpec extends WordSpec with Matchers {

  import EtaReduction.η

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "η-reduction" should {
    "deal with the canonical η-reduction rule" in {
      η(A(λ(x, A(λ(y, y), x)), z)) shouldBe A(λ(y, y), z)
    }
  }
}
