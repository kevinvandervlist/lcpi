package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.ast.lambda.Variable
import nl.soqua.lcpi.interpreter.{Context, InterpreterTester}
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO, ReplState}
import org.scalatest.{Matchers, WordSpecLike}

import scala.language.postfixOps
import scala.util.Try

class ReplScenariosSpec extends ReplMonadTester with InterpreterTester with WordSpecLike with Matchers {

  implicit val emptyContext: Context = emptyState.context
  implicit val state: ReplState = emptyState

  "Someone using the REPL" should {
    val success = new DiskIO with ReplCompiler {
      override def readFile(path: String): Try[Stream[String]] = Try {
        List.empty.toStream
      }
    }
    implicit val compiler = success.compile
    "be able to evaluate a series of expressions that finally yield a single value" in {
      val (_, funcs) = List(
        "FOO := λx.x",
        "BAR := y",
        "BAZ := FOO BAR",
        "BAZ"
      ) parse

      for {
        _ <- ReplMonad.help()
        _ <- ReplMonad.evalExpression(funcs.head)
        _ <- ReplMonad.evalExpression(funcs(1))
        program <- ReplMonad.evalExpression(funcs(2))
      } yield program >> "y"
    }
    "load files transitively" in {
      implicit val compiler = new DiskIO with ReplCompiler {
        override def readFile(path: String): Try[Stream[String]] = Try {
          path match {
            case "foo.lcpi" => List("load bar.lcpi", "load quuk.lcpi").toStream
            case "bar.lcpi" => List("BAR := b", "aschu anth !]8]+{ # rubbish").toStream
            case "quuk.lcpi" => List.empty.toStream
            case _ => List.empty.toStream
          }
        }
      }.compile
      val p = for {
        _ <- ReplMonad.load("foo.lcpi")
        program <- ReplMonad.evalExpression(Variable("BAR"))
      } yield program

      val c = CombinatorLibrary.loadIn(Context()).assign(Variable("BAR"), Variable("b"))
      p >> "b"
      p >> emptyState.copy(context = c, reloadableFiles = List("bar.lcpi", "quuk.lcpi", "foo.lcpi"))
    }
    "loading files and then calling reset should yield a pristine state" in {
      implicit val compiler = new DiskIO with ReplCompiler {
        override def readFile(path: String): Try[Stream[String]] = Try {
          path match {
            case "foo.lcpi" => List("load bar.lcpi").toStream
            case "bar.lcpi" => List("BAR := b").toStream
            case _ => List.empty.toStream
          }
        }
      }.compile
      val p = for {
        _ <- ReplMonad.load("foo.lcpi")
        program <- ReplMonad.reset()
      } yield program

      p >> emptyState
    }
  }
}
