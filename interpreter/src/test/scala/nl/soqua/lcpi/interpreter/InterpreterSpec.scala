package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, WordSpecLike}

class InterpreterSpec extends InterpreterTester with WordSpecLike with Matchers {

  import Expression._

  val x = V("x")
  val y = V("y")
  val z = V("z")

  "Evaluation" should {
    implicit val ctx: Context = Context()
    "stay the same in case of an identity function" in {
      "λx.x" >> λ(x, x)
    }
    "apply a function" in {
      "(λx.x) z" >> z
    }
  }
  "Evaluation with tracing mode" should {
    "Yield a list of intermediate steps" in {
      implicit val ctx: Context = CombinatorLibrary.loadIn(Context())
      val _a = V("a")
      Interpreter.trace(ctx, "(I λx.x) z") match {
        case Left(e) => fail(e.message)
        case Right(expressions) => expressions shouldBe List(
          "S" -> A(A(V("I"), λ(x, x)), z),
          "S" -> A(A(λ(x, x), λ(x, x)), z),
          "α" -> A(A(λ(x, x), λ(_a, _a)), z),
          "β" -> A(λ(_a, _a), z),
          "β" -> z,
          "η" -> z
        )
      }
    }
  }
  "Assignments" should {
    implicit val ctx: Context = Context()
    "do assignments" in {
      "MYVAR := λx.x" >> "λx.x"
      "MYVAR z" >> z
    }
    "but reject them when they already exist" in {
      assertThrows[TestFailedException] {
        "MYVAR := λx.y" >> "λx.y"
      }
      "MYVAR z" >> z
    }
    "deal with parser errors" in {
      assertThrows[TestFailedException] {
        "function(x) { return x; }" >> ""
      }
    }
  }
  "Evaluation with the usage of variables" should {
    implicit val ctx: Context = CombinatorLibrary.loadIn(Context())
    "evaluate using the stored identity function" in {
      "I z" >> z
    }
    "(Y I) is the fixpoint of I" in {
      "Y I" >> "I (Y I)"
    }
  }
}

