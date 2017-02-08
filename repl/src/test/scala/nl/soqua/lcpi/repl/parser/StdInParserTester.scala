package nl.soqua.lcpi.repl.parser

import nl.soqua.lcpi.repl.monad.ReplMonad.Repl
import org.scalatest.Matchers

trait StdInParserTester {

  protected implicit class StdinParserTester(val line: String) extends Matchers {
    def >>(expected: Repl[_]): Unit =
      StdInParser(line).fold(ex => {
        fail(s"Parsing of std in line '$line' failed: $ex")
      }, result => {
        result shouldBe expected
      })
  }

}
