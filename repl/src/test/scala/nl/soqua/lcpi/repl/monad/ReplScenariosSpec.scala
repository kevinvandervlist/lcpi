package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.interpreter.{Context, InterpreterTester}
import nl.soqua.lcpi.repl.lib.DiskIO
import org.scalatest.{Matchers, WordSpecLike}

import scala.language.postfixOps
import scala.util.Try

class ReplScenariosSpec extends ReplMonadTester with InterpreterTester with WordSpecLike with Matchers {

  implicit val emptyContext: Context = emptyState.context

  val success = new DiskIO with ReplCompiler {
    override def readFile(path: String): Try[Stream[String]] = Try {
      List.empty.toStream
    }
  }

  "Someone using the REPL" should {
    implicit val compiler = success.compile
    implicit val state: ReplState = emptyState
    "be able to evaluate a series of expressions that finally yield a single value" in {
      val (_, funcs) = List(
        "FOO := λx.x",
        "BAR := y",
        "BAZ := FOO BAR",
        "BAZ"
      ) parse

      for {
        _ <- ReplMonad.help()
        _ <- ReplMonad.expression(funcs.head)
        _ <- ReplMonad.expression(funcs(1))
        program <- ReplMonad.expression(funcs(2))
      } yield program >> "y"
    }
  }
}
