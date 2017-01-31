package nl.soqua.lcpi.ast

import org.scalatest.{Matchers, WordSpec}

class TermSpec extends WordSpec with Matchers {

  import Term._

  val x = Literal("x")
  val y = Literal("y")

  "Definition of variables" should {
    "be fine" in {
      V(x) shouldBe a[Variable]
    }
    "be identitical" in {
      V(x) shouldBe V(x)
    }
    "have proper equivalence" in {
      val a = V(x)
      val b = V(x)
      val c = V(y)
      a shouldBe b
      b shouldBe a
      a should not be c
      c should not be a
    }
  }
  "Definition of λ" should {
    "be possible from a term and a body" in {
      λ(x, V(x)) shouldBe a[Lambda]
    }
  }
  "Definition of application" should {
    "be possible" in {
      val id = λ(x, V(x))
      A(id, V(y)) shouldBe a[Application]
    }
  }
}
