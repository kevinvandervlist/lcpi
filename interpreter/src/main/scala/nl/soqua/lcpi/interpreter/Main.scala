package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.lambda.Expression.λ
import nl.soqua.lcpi.ast.lambda.Variable

object Main extends App {
  var ctx = Context()

  // TODO: Remove me later on.
  ctx.assign(Variable("I"), λ(Variable("x"), Variable("x")))

  def drawShell(): Unit = print("λ ")

  println(
    """
      |An explanation.
      |
    """.stripMargin
  )

  drawShell()

  for (ln <- io.Source.stdin.getLines) {
    ln match {
      case "quit" => System.exit(0)
      case "show" => ctx.foreach((v, e) => println(s"${Transformation.asString(v)} := ${Transformation.asString(e)}"))
      case "reset" => ctx = Context()
      case _ => Interpreter(ctx, ln) match {
        case Left(e) => System.err.println(s"error: $e")
        case Right(e) => println(Transformation.asString(e))
      }
    }
    drawShell()
  }
}
