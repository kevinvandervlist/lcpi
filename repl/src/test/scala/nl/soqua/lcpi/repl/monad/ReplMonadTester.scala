package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.repl.lib.{DiskIO, ReplState}
import nl.soqua.lcpi.repl.monad.ReplMonad.Repl
import org.scalatest.Matchers

import scala.util.{Failure, Try}

trait ReplMonadTester extends Matchers {

  val emptyState: ReplState = ReplState.empty

  protected implicit class ReplMonadSpecTester(val cmd: Repl[_]) {
    def >>[T](expected: T)(implicit compiler: ReplCompilerDefinition.alias, state: ReplState): Unit = {
      val actual = cmd.foldMap(compiler).run(state).value._2
      actual shouldBe expected
    }

    def >>[T](expected: ReplState)(implicit compiler: ReplCompilerDefinition.alias, state: ReplState): Unit = {
      val actual = cmd.foldMap(compiler).run(state).value._1
      actual shouldBe expected
    }
  }

  val failCompiler = new DiskIO() with ReplCompiler {
    override def readFile(path: String): Try[Stream[String]] = Failure(new IllegalArgumentException("failed"))
  }

}
