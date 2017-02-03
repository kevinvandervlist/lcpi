package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression.λ
import nl.soqua.lcpi.ast.lambda.Variable

object Main extends App {
  var ctx = CombinatorLibrary loadIn Context()

  def drawShell(): Unit = print("λ ")

  println(
    """
      |A λ-calculus interpreter. Type `help` for usage information.
    """.stripMargin
  )

  drawShell()

  for (ln <- io.Source.stdin.getLines) {
    Options(ln) match {
      case Help => println(Options.help)
      case Quit => System.exit(0)
      case Show => ctx.foreach((v, e) => println(s"${Transformation.asString(v)} := ${Transformation.asString(e)}"))
      case Reset => ctx = CombinatorLibrary loadIn Context()
      case Other(l) => Interpreter(ctx, l) match {
        case Left(e) => System.err.println(s"error: $e")
        case Right(e) => println(Transformation.asString(e))
      }
    }
    drawShell()
  }
}
