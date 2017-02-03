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
      |Help:
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