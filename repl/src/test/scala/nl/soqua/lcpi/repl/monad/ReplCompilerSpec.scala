package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.ast.interpreter.Assignment
import nl.soqua.lcpi.ast.lambda.{Application, Variable}
import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.Messages
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO}
import org.scalatest.{Matchers, WordSpecLike}

import scala.util.Try

class ReplCompilerSpec extends ReplMonadTester with WordSpecLike with Matchers {

  "A repl compiler with proper diskio" should {
    val compiler = new DiskIO() with ReplCompiler {
      override def readFile(path: String): Try[Stream[String]] = Try {
        List(
          "FOO := x"
        ).toStream
      }
    }

    implicit val compile: ReplCompilerDefinition.alias = compiler.compile

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
      ReplMonad.show() >> s"       BAR := z${System.lineSeparator()}       FOO := y"
    }
    "reset the environment" in {
      val c = Context().assign(Variable("FOO"), Variable("y"))
      implicit val state = emptyState.copy(context = c)
      ReplMonad.reset() >> emptyState
    }
    "evaluate an expression in normal mode" in {
      val e = Application(Variable("I"), Variable("x"))
      ReplMonad.expression(e) >> "x"
    }
    "evaluate an expression in trace mode" in {
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
    "duplicate assignments should be rejected" in {
      ReplMonad.expression(Assignment(Variable("I"), Variable("x"))) >> "The variable 'I' has already been assigned in the context"
    }
    "load a file" in {
      val c = CombinatorLibrary.loadIn(Context()).assign(Variable("FOO"), Variable("x"))
      ReplMonad.load("foo") >> "Successfully loaded file `foo`"
      ReplMonad.load("foo") >> emptyState.copy(context = c, reloadableFile = Some("foo"))
    }
    "reload a loaded file" in {
      implicit val state = emptyState.copy(reloadableFile = Some("bar"))
      ReplMonad.reload() >> "Successfully reloaded file `bar`"
    }
    "fail to reload when no file is loaded yet" in {
      ReplMonad.reload() >> "Failed to reload: no file has been loaded yet"
    }
  }
  "A repl compiler with failing disk io" should {
    implicit val compiler: ReplCompilerDefinition.alias = failCompiler.compile
    implicit val state: ReplState = emptyState
    "fail to load a file" in {
      ReplMonad.load("foo") >> "Failed to load `foo`: failed"
      ReplMonad.load("foo") >> emptyState
    }
    "load a file, then fail to reload it again" in {
      implicit val state = emptyState.copy(reloadableFile = Some("foo"))
      ReplMonad.reload() >> "Failed to reload `foo`: failed"
    }
  }
}