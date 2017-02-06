package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.interpreter.transformation.{DeBruijnIndex, Stringify}
import org.scalatest.{Matchers, WordSpec}

class ChurchNumeralsSpec extends WordSpec with Matchers {

  import Expression._

  private implicit class ContextBuilder(ctx: Context) extends Matchers {
    def <<(expr: String): Unit = {
      Interpreter(ctx, expr) match {
        case _ =>
      }
    }
  }

  private implicit class Parse(val expr: String) extends Matchers {
    def >>(expectedExpression: Expression)(implicit ctx: Context): Unit = {
      Interpreter(ctx, expr).fold(ex => {
        fail(s"Expression $expr failed: $ex")
      }, actualExpression => {
        withClue(
          s"""
             |expressions are not equal. l >> r:
             |got:      ${Stringify(actualExpression)}
             |expected: ${Stringify(expectedExpression)}
             |---
          """.stripMargin) {
          DeBruijnIndex.index(actualExpression) shouldBe DeBruijnIndex.index(expectedExpression)
        }
      })
    }

    def >>(expectedExpression: String)(implicit ctx: Context): Unit = Interpreter(ctx, expectedExpression)
      .fold(ex => {
        fail(s"Parsing of expected expression $expectedExpression failed: $ex")
      }, >>)
  }

  val x = V("x")
  val y = V("y")
  val z = V("z")
  val xyz = V("xyz")

  "Calculations with Church Numerals" should {
    implicit val ctx: Context = CombinatorLibrary.loadIn(Context())
    ctx << "ZERO := λf.λx.x"
    ctx << "ONE := λf.λx.f x"
    ctx << "TWO := λf.λx.f (f x)"
    ctx << "SUCCESSOR := λn.λf.λx.f (n f x)"
    "Yield number 1 when applying (T 1) 0" in {
      "(T ONE) ZERO" >> "ONE"
    }
    "Yield number 0 when applying (F 0) 1" in {
      "(F ZERO) ONE" >> "ONE"
    }
    "Have a working successor function" in {
      "SUCCESSOR ONE" >> "TWO"
    }
    "evaluate if-then-else properly" in {
      "(((IF T) ONE) TWO)" >> "ONE"
      "(((IF F) ONE) TWO)" >> "TWO"
    }
  }
}

