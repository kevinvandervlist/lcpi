package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{V, λ}
import org.scalatest.{Matchers, WordSpec}

class SubstitutionSpec extends WordSpec with Matchers {

  import Substitution._

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Substitution of expressions" should {
    "substitute y for z" in {
      substitute(λ(x, y), y, z) shouldBe λ(x, z)
    }
  }
}
