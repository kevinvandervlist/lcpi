package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.ast.lambda.{Application, Variable}
import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.Messages
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO}
import nl.soqua.lcpi.repl.monad.ReplMonad.Repl
import org.scalatest.{Matchers, WordSpecLike}

import scala.util.{Failure, Try}

class ReplCompilerSpec extends WordSpecLike with Matchers {

  implicit class ReplMonadSpec(val cmd: Repl[_]) {
    def >>[T](expected: T)(implicit compiler: ReplCompiler.alias, state: ReplState): Unit =
      cmd.foldMap(compiler).run(state).value._2 shouldBe expected

    def >>[T](expected: ReplState)(implicit compiler: ReplCompiler.alias, state: ReplState): Unit =
      cmd.foldMap(compiler).run(state).value._1 shouldBe expected
  }

  val emptyState: ReplState = ReplState.empty

  "A repl compiler" should {
    val failed = new DiskIO {
      override def load(path: String): Try[Stream[String]] = Failure(new IllegalArgumentException("failed"))
    }
    implicit val compiler = ReplCompiler.compiler(new DiskIO {
      override def load(path: String): Try[Stream[String]] = Try {
        List(
          "FOO := x"
        ).toStream
      }
    })
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
    "Load a file" in {
      val c = CombinatorLibrary.loadIn(Context()).assign(Variable("FOO"), Variable("x"))
      ReplMonad.load("foo") >> "Successfully loaded file `foo`"
      ReplMonad.load("foo") >> emptyState.copy(context = c, reloadableFile = Some("foo"))
    }
    "Fail to load a file" in {
      implicit val compiler = ReplCompiler.compiler(failed)
      ReplMonad.load("foo") >> "Failed to load `foo`: failed"
      ReplMonad.load("foo") >> emptyState
    }
    "Reload a loaded file" in {
      implicit val state = emptyState.copy(reloadableFile = Some("bar"))
      ReplMonad.reload() >> "Successfully reloaded file `bar`"
    }
    "Fail to reload when no file is loaded yet" in {
      ReplMonad.reload() >> "Failed to reload: no file has been loaded yet"
    }
  }
}
