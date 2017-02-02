package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Variable
import org.scalatest.{Matchers, WordSpec}

class NamesSpec extends WordSpec with Matchers {

  import Names._

  "Generating unused variable names" should {
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")

    "work when I have seen nothing yet" in {
      unused(x, List.empty) should not be x
    }
    "work when I have some pre-existing context" in {
      val n = unused(x, List(y, z))
      n should not be x
      n should not be y
      n should not be z
    }
    "generate predictable names" in {
      uniqueSuffix(x, List(Variable("x0"), Variable("x1"))) shouldBe Variable("x2")
    }
  }
}
