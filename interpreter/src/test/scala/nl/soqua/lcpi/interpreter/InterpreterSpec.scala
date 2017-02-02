package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpec}

class InterpreterSpec extends WordSpec with Matchers {

  import Expression._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(term: Expression)(implicit ctx: Context): Unit = {
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
    implicit val ctx: Context = Context()
    "stay the same in case of an identity function" in {
      "λx.x" >> λ(x, x)
    }
    "apply a function" in {
      "(λx.x) z" >> z
    }
  }
  "Evaluation with the usage of variables" should {
    "save a function and refer to that when evaluating that" in {
      implicit val ctx: Context = Context()
      ctx.assign(V("I"), λ(x, x))
      "I z" >> z
    }
  }
}
