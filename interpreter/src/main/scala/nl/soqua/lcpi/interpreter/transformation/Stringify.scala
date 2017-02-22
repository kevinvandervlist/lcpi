package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}
import nl.soqua.lcpi.interpreter.show.Show

import scala.language.implicitConversions

object Stringify {

  import Show._

  implicit def expression2String(e: Expression): String = Stringify(e)

  def apply(e: Expression): String = display(Nothing, e)

  private sealed trait ExpressionType

  private case object Function extends ExpressionType

  private case object Argument extends ExpressionType

  // Mark the parent as nothing special
  private case object Nothing extends ExpressionType

  private def display(parent: ExpressionType, e: Expression): ShowS = (parent, e) match {
    case (Function, LambdaAbstraction(_, _)) => parenthesize(display(Nothing, e))
    case (_, LambdaAbstraction(x, a)) => char('λ') compose display(Nothing, x) compose char('.') compose display(Nothing, a)
    case (Function, Application(_: Variable, _: Variable)) => display(Nothing, e)
    case (Function, Application(_, _)) => parenthesize(display(Nothing, e))
    case (Argument, Application(_, _)) => parenthesize(display(Nothing, e))
    case (_, Application(t, s)) => display(Function, t) compose char(' ') compose display(Argument, s)
    case (_, v: Variable) => string(v.symbol)
  }
}
