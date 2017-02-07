package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class VariablesSpec extends WordSpec with Matchers {

  import Variables._

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Identifying free variables in an expression" should {
    "not identify any in the identity function" in {
      free(λ(x, x)) shouldBe Set.empty
    }
    "identify 'y' in a function" in {
      val f = λ(x, y)
      val fv = free(f)
      fv should have size 1
      fv should contain(y)
    }
  }

  "Bound variables" should {
    "be identified as well" in {
      val boundVars = bound(λ(x, λ(y, A(A(x, y), z))))
      boundVars should have size 2
      boundVars should contain allOf(x, y)
    }
    "Asking whether an individual variable is bound should work too" in {
      val e = λ(x, A(x, y))
      isBound(x, e) shouldBe true
      isBound(y, e) shouldBe false
    }
  }

  "Retrieving all variables of expressions" should {
    "discover x and y" in {
      val lambda = λ(x, y)
      vars(lambda) should have size 2
      vars(lambda) should contain allOf(x, y)
    }
    "discover x, y and z" in {
      val lambda = A(λ(x, y), z)
      vars(lambda) should have size 3
      vars(lambda) should contain allOf(x, y, z)
    }
  }
}