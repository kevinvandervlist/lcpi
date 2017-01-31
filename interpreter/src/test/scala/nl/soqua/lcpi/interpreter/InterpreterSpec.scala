package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.{Literal, Term}
import nl.soqua.lcpi.parser.Parser
import org.scalatest.{Matchers, WordSpec}

class InterpreterSpec extends WordSpec with Matchers {

  import nl.soqua.lcpi.ast.Term._

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(term: Term): Unit = {
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

  val x = Literal("x")
  val y = Literal("y")
  val z = Literal("z")
  val xyz = Literal("xyz")

  "Evaluation" should {
    "stay the same in case of an identity function" in {
//      "λx.x" >> λ(x, V(x))
    }
  }
}
