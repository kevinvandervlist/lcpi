package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpecLike}

class ChurchNumeralsSpec extends InterpreterTester with WordSpecLike with Matchers {

  import Expression._

  private implicit class ContextBuilder(ctx: Context) extends Matchers {
    def <<(expr: String): Unit = {
      Interpreter(ctx, expr) match {
        case _ =>
      }
    }
  }

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Calculations with Church Numerals" should {
    implicit val ctx: Context = CombinatorLibrary.loadIn(Context())
    ctx << "ZERO := λf.λx.x"
    ctx << "ONE := λf.λx.f x"
    ctx << "TWO := λf.λx.f (f x)"

    "Yield number 1 when applying (T 1) 0" in {
      "(TRUE ONE) ZERO" >> "ONE"
    }
    "Yield number 0 when applying (F 0) 1" in {
      "(FALSE ZERO) ONE" >> "ONE"
    }

    ctx << "SUCCESSOR := λn.λf.λx.f (n f x)"
    "Have a working successor function" in {
      "SUCCESSOR ONE" >> "TWO"
    }
    "evaluate if-then-else properly" in {
      "(((IF TRUE) ONE) TWO)" >> "ONE"
      "(((IF FALSE) ONE) TWO)" >> "TWO"
    }

    ctx << "ISZERO := λn.n (λx.FALSE) TRUE"
    "determine whether `zero` is actually zero" in {
      "ISZERO ZERO" >> "λx.(λy.x)"
    }
    "determine that `one` is not zero" in {
      "ISZERO ONE" >> "FALSE"
    }
    
  }
}

