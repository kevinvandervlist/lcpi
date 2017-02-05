package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.A
import nl.soqua.lcpi.ast.lambda.{Expression, LambdaAbstraction, Variable}
import org.scalatest.{Matchers, WordSpec}

class DeBruijnIndexSpec extends WordSpec with Matchers {

  import DeBruijnIndex._

  val _a = Expression.V("a")
  val b = Expression.V("b")
  val c = Expression.V("c")
  val d = Expression.V("d")

  val x = Expression.V("x")
  val y = Expression.V("y")
  val z = Expression.V("z")

  private def λ(v: Variable, e: Expression): LambdaAbstraction = Expression.λ(v, e)

  private def λ(e: Expression): LambdaAbstraction = Expression.λ(Expression.V(""), e)

  private def V(cnt: Int): Variable = Expression.V(cnt.toString)

  val _1 = V(1)
  val _2 = V(2)

  "Transforming to De Bruijn indexes" should {
    "work on the identity function" in {
      index(λ(x, x)) shouldBe λ(_1)
    }
    "work with church truth" in {
      index(λ(x, λ(y, x))) shouldBe λ(λ(_2))
    }
    "deal with a free variable" in {
      index(λ(x, y)) shouldBe λ(_2)
    }
    "work on a more complex function" in {
      val actual = index(λ(z, A(λ(y, A(y, λ(x, x))), λ(x, A(z, x)))))
      val expected = λ(A(λ(A(_1, λ(_1))), λ(A(_2, _1))))
      actual shouldBe expected
    }
  }
  "Reification names based on De Bruijn indexed lambdas" should {
    "work on the identity function" in {
      reify(λ(_1)) shouldBe λ(_a, _a)
    }
    "work with church truth" in {
      reify(λ(λ(_2))) shouldBe λ(_a, λ(b, _a))
    }
    "deal with a free variable" in {
      reify(λ(_2)) shouldBe λ(_a, b)
    }
    "work on a more complex function" in {
      val actual = reify(λ(A(λ(A(_1, λ(_1))), λ(A(_2, _1)))))
      val expected = λ(_a, A(λ(b, A(b, λ(c, c))), λ(d, A(_a, d))))
      actual shouldBe expected
    }
  }
}
