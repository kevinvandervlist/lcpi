package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.{Matchers, WordSpecLike}

class InterpreterResultSpec extends InterpreterTester with WordSpecLike with Matchers {

  import Expression._

  "An InterpreterResult" should {
    val ctx = Context()
    val x = V("x")
    val y = V("y")
    val expr1 = λ(x, x)
    val expr2 = λ(y, y)
    "without a trace should be mappend" in {
      InterpreterResult(ctx, expr1).map(_ => expr2) shouldBe InterpreterResult(ctx, expr2)
    }
    "with a trace should be mappend" in {
      val intrace = List("_" -> expr1, "_" -> expr2)
      val expected = TraceInterpreterResult(ctx, expr1, intrace)
      InterpreterResult(ctx, intrace).map(_ => expr1) shouldBe expected
    }
  }
}
