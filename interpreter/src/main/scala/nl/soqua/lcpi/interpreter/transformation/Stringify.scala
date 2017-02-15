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

  // See commit 165ee2c79a91fa03d90a37e06804f4366f028942 for details on why the reverse is applied.
  def apply(e: Expression): String = display(Nothing, e)(StringBuilder.newBuilder).toString().reverse

  def display(parent: ExpressionType, e: Expression): StringBuilder => StringBuilder = (parent, e) match {
    case (Function, LambdaAbstraction(_, _)) => parenthesize(display(Nothing, e))
    case (_, LambdaAbstraction(x, a)) => lambda compose display(Nothing, x) compose dot compose display(Nothing, a)
    case (Function, Application(_: Variable, _: Variable)) => display(Nothing, e)
    case (Function, Application(_, _)) => parenthesize(display(Nothing, e))
    case (Argument, Application(_, _)) => parenthesize(display(Nothing, e))
    case (_, Application(t, s)) => display(Function, t) compose space compose display(Argument, s)
    case (_, v: Variable) => show(v.symbol)
  }

  @inline private def show(s: String): StringBuilder => StringBuilder = (b: StringBuilder) => b.append(s.reverse)

  @inline private def space = show(" ")

  @inline private def lambda = show("λ")

  @inline private def dot = show(".")

  @inline private def parenthesize(f: StringBuilder => StringBuilder): StringBuilder => StringBuilder =
    show("(") compose f compose show(")")
}
