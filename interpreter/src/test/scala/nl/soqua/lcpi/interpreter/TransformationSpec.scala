package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.Expression.{V, λ, A}
import org.scalatest.{Matchers, WordSpec}

class TransformationSpec extends WordSpec with Matchers {
  import Transformation._

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Identifying free variables in an expression" should {
    "not identify any in the identity function" in {
      free(λ(x, x)) shouldBe List.empty
    }
    "identify 'y' in a function" in {
      val f = λ(x, y)
      val fv = free(f)
      fv should have length 1
      fv should contain (y)
    }
  }
  "Substitution of expressions" should {
    "substitute y for z" in {
      sub(λ(x, y), y, z) shouldBe λ(x, z)
    }
  }
  "Retrieving all variables of expressions" should {
    "discover x and y" in {
      val lambda = λ(x, y)
      vars(lambda) should have length 2
      vars(lambda) should contain allOf (x, y)
    }
    "discover x, y and z" in {
      val lambda = A(λ(x, y), z)
      vars(lambda) should have length 3
      vars(lambda) should contain allOf (x, y, z)
    }
  }
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
