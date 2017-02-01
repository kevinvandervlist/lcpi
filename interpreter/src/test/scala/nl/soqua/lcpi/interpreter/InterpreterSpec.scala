package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.Expression
import nl.soqua.lcpi.parser.Parser
import org.scalatest.{Matchers, WordSpec}

class InterpreterSpec extends WordSpec with Matchers {

  import nl.soqua.lcpi.ast.Expression._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(term: Expression): Unit = {
      val ctx: Context = ???
      val ires = for {
        p <- Parser(expr)
        i <- Interpreter(ctx, p)
      } yield i
      ires.fold(ex => {
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
//      "λx.x" >> λ(x, V(x))
    }
  }
}
