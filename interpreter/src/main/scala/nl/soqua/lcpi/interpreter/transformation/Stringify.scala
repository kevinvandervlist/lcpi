package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.{Application, Expression, LambdaAbstraction, Variable}

import scala.language.implicitConversions

object Stringify {
  implicit def expression2String(e: Expression): String = Stringify(e)

  private sealed trait ExpressionType

  private case object Function extends ExpressionType

  private case object Argument extends ExpressionType

  // Mark the parent as nothing special
  private case object Nothing extends ExpressionType

  def apply(e: Expression): String = display(Nothing, e)("")

  private def display(parent: ExpressionType, e: Expression): String => String = (parent, e) match {
    case (Function, LambdaAbstraction(_, _)) => parenthesize(display(Nothing, e))
    case (_, LambdaAbstraction(x, a)) => lambda compose display(Nothing, x) compose dot compose display(Nothing, a)
    case (Function, Application(_: Variable, _: Variable)) => display(Nothing, e)
    case (Function, Application(_, _)) => parenthesize(display(Nothing, e))
    case (Argument, Application(_, _)) => parenthesize(display(Nothing, e))
    case (_, Application(t, s)) => display(Function, t) compose space compose display(Argument, s)
    case (_, v: Variable) => show(v.symbol)
  }

  private def show(s: String): String => String = (suffix: String) =>
    s"$s$suffix"

  private def space = show(" ")

  private def lambda = show("λ")

  private def dot = show(".")

  private def parenthesize(f: String => String): String => String =
    show("(") compose f compose show(")")
}
