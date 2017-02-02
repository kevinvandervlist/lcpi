package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpec}

class InterpreterSpec extends WordSpec with Matchers {

  import Expression._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(term: Expression): Unit = {
      val ctx: Context = Context()
      Interpreter(ctx, expr).fold(ex => {
        fail(s"Expression $expr failed: $ex")
      }, res => {
        res shouldBe term
      })
    }
  }

  val x = V("x")
  val y = V("y")
  val z = V("z")
  val xyz = V("xyz")

  "Evaluation" should {
    "stay the same in case of an identity function" in {
      "λx.x" >> λ(x, x)
    }
    "apply a function" in {
      "(λx.x) z" >> z
    }
  }
}
