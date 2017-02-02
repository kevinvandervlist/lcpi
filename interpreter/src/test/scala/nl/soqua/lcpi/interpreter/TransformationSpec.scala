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
      substitute(λ(x, y), y, z) shouldBe λ(x, z)
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
      val lambda = A(λ(x, A(x, x)), λ(x,A(x, x)))
      val reduced = β(lambda)
      reduced shouldBe lambda
    }
    "do multi-step β reduction" in {
      β(A(λ(x, x), A(λ(x, x), x))) shouldBe x
    }
    "work with a complex example as well" in {
      val reduced = β(A(A(λ(x, λ(y, A(y, x))), λ(x, x)), V("1")))
      println(reduced)
    }
  }
}
