package nl.soqua.lcpi.repl

import nl.soqua.lcpi.interpreter.{Context, Interpreter}
import nl.soqua.lcpi.interpreter.transformation.Stringify

object Main extends App {
  var ctx = CombinatorLibrary loadIn Context()

  def drawShell(): Unit = print("lcpi λ> ")

  println(
    """
      |A λ-calculus interpreter. Type `help` for usage information.
    """.stripMargin
  )

  drawShell()

  var traceMode = false

  for (ln <- io.Source.stdin.getLines) {
    Options(ln) match {
      case Help => println(Options.help)
      case Quit => System.exit(0)
      case Show => ctx.foreach((v, e) => println(s"${Stringify(v)} := ${Stringify(e)}"))
      case Reset => ctx = CombinatorLibrary loadIn Context()
      case TraceMode if traceMode =>
        println("Trace mode is now off.")
        traceMode = false
      case TraceMode if !traceMode =>
        println("Trace mode is now on.")
        traceMode = true
      case Other(l) if !traceMode => Interpreter(ctx, l) match {
        case Left(e) => System.err.println(s"error: ${e.message}")
        case Right(e) => println(Stringify(e))
      }
      case Other(l) if traceMode => Interpreter.trace(ctx, l) match {
        case Left(e) => System.err.println(s"error: ${e.message}")
        case Right(e) => e.foreach {
          case (s, expr) => println(s"$s => ${Stringify(expr)}")
        }
      }
    }
    drawShell()
  }
}
