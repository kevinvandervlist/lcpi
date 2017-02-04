package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.A
import nl.soqua.lcpi.ast.lambda.{Expression, LambdaAbstraction, Variable}
import org.scalatest.{Matchers, WordSpec}

class DeBruijnIndexSpec extends WordSpec with Matchers {

  import DeBruijnIndex._

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
    "church truth" in {
      index(λ(x, λ(y, x))) shouldBe λ(λ(_2))
    }
    "work on a more complex function" in {
      val actual = index(λ(z, A(λ(y, A(y, λ(x, x))), λ(x, A(z, x)))))
      val expected = λ(A(λ(A(_1, λ(_1))), λ(A(_2, _1))))
      actual shouldBe expected
    }
  }
  "Reification names based on De Bruijn indexed lambdas" should {
    "work on the identity function" in {
      reification(λ(_1)) shouldBe λ(x, x)
    }
    "church truth" in {
      reification(λ(λ(_2))) shouldBe λ(x, λ(y, x))
    }
    "work on a more complex function" in {
      val actual = reification(λ(A(λ(A(_1, λ(_1))), λ(A(_2, _1)))))
      val expected = λ(z, A(λ(y, A(y, λ(x, x))), λ(x, A(z, x))))
      actual shouldBe expected
    }
  }
}
