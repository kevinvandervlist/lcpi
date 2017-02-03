package nl.soqua.lcpi.interpreter

sealed trait Options

object Options {
  def apply(str: String): Options = str match {
    case "quit" | "exit" => Quit
    case "help" => Help
    case "show" => Show
    case "reset" => Reset
    case _ => Other(str)
  }

  def help: String =
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
      |* `reset` => reset the current REPL context.
    """.stripMargin

}

case object Help extends Options

case object Quit extends Options

case object Show extends Options

case object Reset extends Options

case class Other(line: String) extends Options