package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class TransformationSpec extends WordSpec with Matchers {

  import Transformation._

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
  }
  "Substitution of expressions" should {
    "substitute y for z" in {
      sub(λ(x, y), y, z) shouldBe λ(x, z)
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
  "β-reduction" should {
    "work with a simple function" in {
      β(A(λ(x, x), x)) shouldBe x
    }
    "work with (λx.x λ(y y))" in {
      val reduced = β(A(λ(x, x), λ(y,y)))
      println(reduced)
    }
    "work with a complex example as well" in {
      val reduced = β(A(A(λ(x, λ(y, A(y, x))), λ(x, x)), V("1")))
      println(reduced)
    }
  }
}
