package nl.soqua.lcpi.interpreter.show

import org.scalatest.{Matchers, WordSpec}

class ShowSpec extends WordSpec with Matchers {
  import Show._

  "Show" should {
    "render an empty string" in {
      val str: String = Show.empty
      str shouldBe ""
    }
    "composing values should be ok" in {
      val str: String = string("abc") compose string("def")
      str shouldBe "abcdef"
    }
    "composing with empty is fine as well" in {
      val str: String = Show.empty compose string("abc") compose Show.empty compose string("def") compose Show.empty
      str shouldBe "abcdef"
    }
    "wrap a composition with parentheses" in {
      val f = string("a") compose string("z")
      val g = parenthesize apply f
      val parenized: String = string("_") compose g
      parenized shouldBe "_(az)"
    }
  }
}
