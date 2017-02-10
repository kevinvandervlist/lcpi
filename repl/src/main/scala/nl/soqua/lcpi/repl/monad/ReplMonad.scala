package nl.soqua.lcpi.repl.monad

import cats.free.Free
import nl.soqua.lcpi.ast.interpreter.ReplExpression

sealed trait ReplMonadA[A]

case object Help extends ReplMonadA[String]

case object Quit extends ReplMonadA[Unit]

case object Show extends ReplMonadA[String]

case object Reset extends ReplMonadA[Unit]

case object TraceMode extends ReplMonadA[String]

case class LoadFile(path: String) extends ReplMonadA[String]

case object Reload extends ReplMonadA[String]

case class Command(expression: ReplExpression) extends ReplMonadA[String]

object ReplMonad {
  type Repl[A] = Free[ReplMonadA, A]

  def help(): Repl[String] = Free.liftF[ReplMonadA, String](Help)

  def quit(): Repl[Unit] = Free.liftF[ReplMonadA, Unit](Quit)

  def show(): Repl[String] = Free.liftF[ReplMonadA, String](Show)

  def reset(): Repl[Unit] = Free.liftF[ReplMonadA, Unit](Reset)

  def trace(): Repl[String] = Free.liftF[ReplMonadA, String](TraceMode)

  def load(path: String): Repl[String] = Free.liftF[ReplMonadA, String](LoadFile(path))

  def reload(): Repl[String] = Free.liftF[ReplMonadA, String](Reload)

  def expression(e: ReplExpression): Free[ReplMonadA, String] = Free.liftF[ReplMonadA, String](Command(e))

}