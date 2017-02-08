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
    implicit val ctx: Context = Context()
    ctx << "ZERO := λf.λx.x"
    ctx << "SUCCESSOR := λn.λf.λx.f (n f x)"
    ctx << "ONE := SUCCESSOR ZERO"
    ctx << "TWO := SUCCESSOR ONE"
    ctx << "THREE := SUCCESSOR TWO"

    ctx << "TRUE := λx.λy.x"
    ctx << "FALSE := λx.λy.y"
    ctx << "IF := λp.λi.λe.p i e"

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

    ctx << "ISZERO := λn.n (λx.FALSE) TRUE"
    "determine whether `zero` is actually zero" in {
      "ISZERO ZERO" >> "λx.(λy.x)"
    }
    "determine that `one` is not zero" in {
      "ISZERO ONE" >> "FALSE"
    }

    ctx << "PLUS := λm.λn.m SUCCESSOR n"
    ctx << "PREDECESSOR := λn.n (λg.λk.ISZERO (g ONE) k (PLUS (g k) ONE)) (λv.ZERO) ZERO"
    "determine that `1` is a predecessor of `2`" in {
      "PREDECESSOR TWO" >> "ONE"
    }

    ctx << "Y := λf.(λx.f (x x)) (λx.f (x x))"
    ctx << "PARTIALSUMMATION := (λf.λn.(ISZERO n) ZERO (PLUS n (f (PREDECESSOR n))))"
    ctx << "SUMMATION := Y PARTIALSUMMATION"
    "determine that the summation of `S(0, 3)` is of `6`" in {
      "SUMMATION THREE" >> "PLUS THREE THREE"
    }
  }
}

