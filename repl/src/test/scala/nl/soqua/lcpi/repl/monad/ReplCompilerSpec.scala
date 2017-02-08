package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.ast.lambda.{Application, LambdaAbstraction, Variable}
import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.Messages
import nl.soqua.lcpi.repl.monad.ReplMonad.Repl
import org.scalatest.{Matchers, WordSpecLike}

class ReplCompilerSpec extends WordSpecLike with Matchers {

  implicit class ReplMonadSpec(val cmd: Repl[_]) {
    def >>[T](expected: T)(implicit state: ReplState): Unit =
      cmd.foldMap(ReplCompiler.pureCompiler).run(state).value._2 shouldBe expected

    def >>[T](expected: ReplState)(implicit state: ReplState): Unit =
      cmd.foldMap(ReplCompiler.pureCompiler).run(state).value._1 shouldBe expected
  }

  val emptyState: ReplState = ReplState.empty

  "A repl compiler" should {
    implicit val state: ReplState = emptyState
    "provide the user with usage information" in {
      ReplMonad.help() >> Messages.help
    }
    "quit if asked" in {
      ReplMonad.quit() >> emptyState.copy(terminated = true)
    }
    "toggle trace mode on" in {
      ReplMonad.trace() >> emptyState.copy(traceMode = Enabled)
      ReplMonad.trace() >> Messages.traceModeEnabled
    }
    "toggle trace mode off" in {
      implicit val state: ReplState = emptyState.copy(traceMode = Enabled)
      ReplMonad.trace() >> emptyState.copy(traceMode = Disabled)
      ReplMonad.trace() >> Messages.traceModeDisabled
    }
    "show the current environment if asked for" in {
      val c = Context()
        .assign(Variable("FOO"), Variable("y"))
        .assign(Variable("BAR"), Variable("z"))
      implicit val state = emptyState.copy(context = c)
      ReplMonad.show() >> s"FOO := y${System.lineSeparator()}BAR := z"
    }
    "reset the environment" in {
      val c = Context().assign(Variable("FOO"), Variable("y"))
      implicit val state = emptyState.copy(context = c)
      ReplMonad.reset() >> emptyState
    }
    "Evaluate an expression in normal mode" in {
      val e = Application(Variable("I"), Variable("x"))
      ReplMonad.expression(e) >> "x"
    }
    "Evaluate an expression in trace mode" in {
      implicit val state = emptyState.copy(traceMode = Enabled)
      val e = Application(Variable("I"), Variable("x"))
      ReplMonad.expression(e) >> List(
        "S => ((λx.x) x)",
        "α => ((λx.x) x)",
        "β => x",
        "η => x",
        "x"
      ).mkString(System.lineSeparator())
    }
    "Duplicate assignments should be rejected" in {
      ReplMonad.expression(Assignment(Variable("I"), Variable("x"))) >> "The variable 'I' has already been assigned in the context"
    }
  }
}
