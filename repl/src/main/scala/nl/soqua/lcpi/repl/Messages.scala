package nl.soqua.lcpi.repl

import java.io.File

import scala.util.Try

object Messages {

  private def relativePath: String = Try {
    new File(".").getAbsolutePath
  }.getOrElse("unknown")

  val help: String =
    s"""
      |Notation:
      |* λ is either a λ or a \\. So both \\x.x and λx.x are allowed.
      |* You can assign variables; `I := λx.x` stores the identity function to the variable `I`.
      |  These have to be capitalized.
      |
      |Available commands:
      |* `help` => show this message. Alias: `exit`.
      |* `quit` => quit the REPL.
      |* `show` => show the current REPL context.
      |* `load <name>` => load a file based on the relative path ($relativePath). Lines starting with `#` are ignored.
      |* `reload` => reload all files, overwriting any existing variables in the context if they already exist.
      |* `trace` => toggle trace mode.
      |* `reset` => reset the current REPL context.
      |* `<λ-expression>` => evaluate a λ-expression.
      |* `dbi <λ-expression>` => evaluate a λ-expression and render the result in De Bruijn Index notation.
    """.stripMargin
  val traceModeEnabled: String = "Trace mode is now on."
  val traceModeDisabled: String = "Trace mode is now off."
}
