package nl.soqua.lcpi.repl.parser

import nl.soqua.lcpi.parser.ParserError
import nl.soqua.lcpi.parser.repl.ReplParser
import nl.soqua.lcpi.repl.monad.ReplMonad
import nl.soqua.lcpi.repl.monad.ReplMonad.Repl

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

trait StdInParserRules extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]

  override val whiteSpace: Regex = "[\r\n]+".r

  lazy val line: P[Repl[_]] = {
    help | quit | show | reset | trace | load | reload | deBruijnIndex | command
  }

  lazy val help: P[Repl[_]] = {
    "help".r ^^ (_ => ReplMonad.help())
  }

  lazy val quit: P[Repl[_]] = {
    "quit".r ^^ (_ => ReplMonad.quit())
  }

  lazy val show: P[Repl[_]] = {
    "show".r ^^ (_ => ReplMonad.show())
  }

  lazy val reset: P[Repl[_]] = {
    "reset".r ^^ (_ => ReplMonad.reset())
  }

  lazy val trace: P[Repl[_]] = {
    "trace".r ^^ (_ => ReplMonad.trace())
  }

  lazy val load: P[Repl[_]] = {
    "load".r ~> "\\s+".r ~> "[a-zA-Z0-9_.-]+".r ^^ (p => ReplMonad.load(p))
  }

  lazy val reload: P[Repl[_]] = {
    "reload".r ^^ (_ => ReplMonad.reload())
  }

  lazy val deBruijnIndex: P[Repl[_]] = {
    "dbi".r ~> ".*".r >> (v => ReplParser(v) match {
      case Left(e) => failure(e.message)
      case Right(e) => success(ReplMonad.deBruijnIndex(e))
    })
  }

  lazy val command: P[Repl[_]] = {
    ".*".r >> (v => ReplParser(v) match {
      case Left(e) => failure(e.message)
      case Right(e) => success(ReplMonad.evalExpression(e))
    })
  }
}

object StdInParser extends StdInParserRules {
  def apply(source: String): Either[ParserError, Repl[_]] = {
    parse(parseLine, new PackratReader(new CharSequenceReader(source))) match {
      case NoSuccess(msg, _) => Left(ParserError(msg))
      case Success(p, _) => Right(p)
    }
  }

  lazy val parseLine: P[Repl[_]] = phrase(line)

}
