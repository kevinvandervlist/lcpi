package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.interpreter.transformation.{DeBruijnIndex, Stringify}
import org.scalatest.{Matchers, WordSpec, WordSpecLike}

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
    ctx << "SUCCESSOR := λn.λf.λx.f (n f x)"
    "Yield number 1 when applying (T 1) 0" in {
      "(TRUE ONE) ZERO" >> "ONE"
    }
    "Yield number 0 when applying (F 0) 1" in {
      "(FALSE ZERO) ONE" >> "ONE"
    }
    "Have a working successor function" in {
      "SUCCESSOR ONE" >> "TWO"
    }
    "evaluate if-then-else properly" in {
      "(((IF TRUE) ONE) TWO)" >> "ONE"
      "(((IF FALSE) ONE) TWO)" >> "TWO"
    }
  }
}

