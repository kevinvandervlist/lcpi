package nl.soqua.lcpi.repl

object Messages {
  val help: String =
    """
      |Notation:
      |* λ is either a λ or a \. So both \x.x and λx.x are allowed.
      |* You can assign variables; `I := λx.x` stores the identity function to the variable `I`.
      |  These have to be capitalized.
      |
      |Available commands:
      |* `help` => show this message. Alias: `exit`.
      |* `quit` => quit the REPL.
      |* `show` => show the current REPL context.
      |* `trace` => toggle trace mode
      |* `reset` => reset the current REPL context.
    """.stripMargin
  val traceModeEnabled: String = "Trace mode is now on."
  val traceModeDisabled: String = "Trace mode is now off."
}
